/*package scala.virtualization.lms
package common

import java.io._
import reflect.ClassManifest
import scala.collection.mutable.HashMap
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util._
import scala.reflect.SourceContext

object DynamicRecordsMap extends Serializable {
    // We want to be able to call the scala compiler without generating code
    var mapInitialized: Boolean = false;
    var dataPath: String = null;
    var registeredDynamicRecords: HashMap[String, List[(String, Class[_])]] = null

    // Helpers for printing information
    def getNames(recname: String) = {
        registeredDynamicRecords(recname).map(x => x._1).mkString("|")
    }

    def initialize(rootFolder: String) = {
        if (mapInitialized)
            throw new RuntimeException("DynamicRecordsMap already initialized");
        mapInitialized = true
        dataPath = rootFolder 
        registeredDynamicRecords = new HashMap[String, List[(String, Class[_])]]
    }

}

object DynamicRecordEffectsMap {
    val effectsMap = new collection.mutable.HashMap[(String,String), Any]()
}

trait DynamicRecord extends Base with Serializable with VariablesExp {

	object NewDynamicRecord {
    	def apply[T:Manifest](n: String, reuse: Boolean = false) = newDynamicRecord(n, reuse)    
	}

	class DynamicRecordOps(x: Rep[DynamicRecord]) {
        def get(field: Rep[Any]) = dynamicRecordGet(x, field)
		def set(field: Rep[Any], value: Rep[Any]) = dynamicRecordSet(x, field, value)
        def foreach(f: Rep[DynamicRecord] => Rep[Unit]) = dynamicRecordForEach(x,f)
    }
	implicit def varDynamicType2dynamicRecordOps(x: Var[DynamicRecord]) = new DynamicRecordOps(readVar(x))
	implicit def varDynamicType2dynamicRecord(x: Var[DynamicRecord]) = readVar(x)
	implicit def dynamicRecord2dynamicRecordOps(x: Rep[DynamicRecord]) = new DynamicRecordOps(x)
	implicit def dynamicRecord2RepdynamicRecordOps(x: DynamicRecord) = new DynamicRecordOps(unit(x))

    def newDynamicRecord(name: String, reuse: Boolean = false): Rep[DynamicRecord]
	def dynamicRecordGet(x: Rep[DynamicRecord], field: Rep[Any]): Rep[Any]
	def dynamicRecordSet(x: Rep[DynamicRecord], field: Rep[Any], value: Rep[Any]): Rep[Unit]
    def dynamicRecordForEach(x: Rep[DynamicRecord], f: Rep[DynamicRecord] => Rep[Unit]): Rep[Unit]
    val NullDynamicRecord = unit(null).asInstanceOf[Rep[DynamicRecord]]
}

trait DynamicRecordExp extends DynamicRecord with BaseExp with EffectExp {
    case class NewDynamicRecordObj(n: String) extends Def[DynamicRecord]
	case class DynamicRecordGet(x: Rep[DynamicRecord], field: Rep[Any]) extends Def[Any]
	case class DynamicRecordSet(x: Rep[DynamicRecord], field: Rep[Any], value: Rep[Any]) extends Def[Unit]
    case class DynamicRecordForEach(l: Rep[DynamicRecord], x: Sym[DynamicRecord], block: Block[Unit]) extends Def[Unit]

    def newDynamicRecord(name: String, reuse: Boolean = false) = 
        if (reuse) NewDynamicRecordObj(name) else reflectMutable(NewDynamicRecordObj(name))

	def dynamicRecordGet(x: Rep[DynamicRecord], field: Rep[Any]) = {
        val key = (x.toString, field.toString)
        DynamicRecordEffectsMap.effectsMap.get(key) match {
            case Some(e) => e.asInstanceOf[Sym[_]]
            case None => {
                val rE = reflectWrite(x)(DynamicRecordGet(x, field))
                DynamicRecordEffectsMap.effectsMap += key -> rE
                rE
            }
        }
    }

	def dynamicRecordSet(x: Rep[DynamicRecord], field: Rep[Any], value: Rep[Any]) = { 
        val key = (x.toString, field.toString)
        // Forces new gets that follow to re-read the entry.
        val e = DynamicRecordEffectsMap.effectsMap.remove(key) 
        reflectEffect(DynamicRecordSet(x,field,value))
    }

    def dynamicRecordForEach(x: Rep[DynamicRecord], f: Exp[DynamicRecord] => Exp[Unit])={
        val a = fresh[DynamicRecord]
        val b = reifyEffects(f(a))
        reflectEffect(DynamicRecordForEach(x, a, b), summarizeEffects(b).star)
    }
    
    override def syms(e: Any): List[Sym[Any]] = e match {
        case DynamicRecordForEach(a, x, body) => syms(a):::syms(body)
        case _ => super.syms(e)
    }

    override def boundSyms(e: Any): List[Sym[Any]] = e match {
        case DynamicRecordForEach(a, x, body) => x :: effectSyms(body)
        case _ => super.boundSyms(e)
    }

    override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
        case DynamicRecordForEach(a, x, body) => freqNormal(a):::freqHot(body)
        case _ => super.symsFreq(e)
    }  
}

trait ScalaGenDynamicRecord extends ScalaGenBase with GenericNestedCodegen {
	val IR: DynamicRecordExp
	import IR._
 
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) =  { 
        rhs match {
            case NewDynamicRecordObj(x) => emitValDef(sym, "new " + x + "()")
		    case DynamicRecordGet(x, field) => emitValDef(sym, quote(x) + "." + quote(field).replaceAll("\"",""))
		    case DynamicRecordSet(x, field, value) =>
                stream.println(quote(x) + "." + quote(field).replaceAll("\"","") + " = " + quote(value))
            case DynamicRecordForEach(x, init, block) => 
                stream.println("val x" + sym.toString.replace("Sym(","").replace(")","") + " = {")
                stream.println("\tvar " + quote(init) + "=" + quote(x))
                stream.println("\twhile (" + quote(init) + " != null) {")
                emitBlock(block)
                stream.println("\t\t" + quote(init) + "=" + quote(init) + ".next")
                stream.println("\t}")
                stream.println("}")
                       
		    case _ => super.emitNode(sym, rhs)
  	    }
    }

    override def emitDataStructures(out: PrintWriter) {
        DynamicRecordsMap.registeredDynamicRecords.foreach(
            rec => writeRecord(out, rec._1, rec._2)
        )
        //DynamicRecordsMap.registeredDynamicRecords.clear
    }

	def writeRecord(out: PrintWriter, className: String, attrs: List[(String, Class[_])]) {
        out.print("class " + className + " extends Serializable" +" {\n")
        for ((p1,p2) <- attrs) {
             val str = {
                 if (ClassManifest.fromClass(p2) == classManifest[Array[Byte]])
                    "Array[Byte]"
                 else 
                    p2.toString.replaceAll("class ","").replaceAll("int", "scala.Int").replaceAll("double","scala.Double").replaceAll("char", "Char").replaceAll("long", "scala.Long")
             }
             out.print("private[this] final var _" + p1 + ": " + str + " = null.asInstanceOf[" + str + "];\n")
			 out.println("@inline final def " + p1 + " = _" + p1)
			 out.println("@inline final def " + p1 + "_=(x:" + str + ") = _" + p1 + " = x ")
        }
        out.println("@transient var next: " + className + " = null ")
        // Custom toString function
        out.print("override def toString() = {\n\"\"+")
        out.print( (for ((p1,p2) <- attrs) yield 
            if (ClassManifest.fromClass(p2) == classManifest[Array[Byte]]) "new String(" + p1 + ")" 
            else if (p1.contains("DATE")) "{new java.util.Date(" + p1 + ")}"
            else p1
        ).mkString("+\"|\"+") )
        out.print("}\n");
        // Custom serialization/deserialization routines
        out.println("@throws(classOf[java.io.IOException])")
        out.println("private def writeObject(out: java.io.ObjectOutputStream): Unit = {")
        for ((p1,p2) <- attrs) {
            ClassManifest.fromClass(p2) match {
                case m if m <:< classManifest[scala.Int]        => out.println("out.writeInt(" + p1 + ")")
                case m if m <:< classManifest[scala.Double]     => out.println("out.writeDouble(" + p1 + ")")
                case m if m <:< classManifest[scala.Char]       => out.println("out.writeChar(" + p1 + ")")
                case m if m <:< classManifest[scala.Long]       => out.println("out.writeLong(" + p1 + ")")
                case m if m <:< classManifest[java.lang.String] => out.println("out.writeUTF(" + p1 + ")")
                case m if m == classManifest[Array[Byte]]       => {
                    out.println("out.writeInt(" + p1 +".length)")
                    out.println("out.write(" + p1 + ")")
                }
                case _ => out.println("out.writeObject(" + p1 + ")")
            }
        }
        out.println("}")
        out.println("@throws(classOf[java.io.IOException])")
        out.println("private def readObject(in: java.io.ObjectInputStream): Unit = {")
        out.println("var length: Int = 0");
        for ((p1,p2) <- attrs) {
            ClassManifest.fromClass(p2) match {
                case m if m <:< classManifest[scala.Int]        => out.println(p1 + " = in.readInt()")
                case m if m <:< classManifest[scala.Double]     => out.println(p1 + " = in.readDouble()")
                case m if m <:< classManifest[scala.Char]       => out.println(p1 + " = in.readChar()")
                case m if m <:< classManifest[scala.Long]       => out.println(p1 + " = in.readLong()")
                case m if m <:< classManifest[java.lang.String] => out.println(p1 + " = in.readUTF()")
                case m if m == classManifest[Array[Byte]]       => {
                    out.println(p1 + " = new Array[Byte](in.readInt())")
                    out.println("in.read(" + p1 + ", 0, " + p1 + ".length)")
                }
                case _ => out.println(p1 + " = in.readObject().asInstanceOf[" + p2.toString.replaceAll("class ","") + "]")
            }
        }
        out.println("}")
        out.println("}") // End of class
        out.flush
    }

	def newDynamicRecordType(name: String, attrs: List[(String, Class[_])]) {
        if (DynamicRecordsMap.registeredDynamicRecords.get(name) == None) {
            // Register for first use
            DynamicRecordsMap.registeredDynamicRecords += (name -> attrs)
            // Write to file (for persistence)
			val filename = DynamicRecordsMap.dataPath + name + ".scala"
            val writer = new PrintWriter(new java.io.File(filename))
            writeRecord(writer, name, attrs)
            writer.close()
        }
    }

}

trait CGenDynamicRecord extends CGenBase with GenericNestedCodegen {
	val IR: DynamicRecordExp
	import IR._
 
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) =  { 
		rhs match {
    	    case NewDynamicRecordObj(x) => allocStruct(sym, x, stream)
			case DynamicRecordGet(x, field) => emitValDef(sym, quote(x) + "->" + quote(field).replaceAll("\"",""))
			case DynamicRecordSet(x, field, value) =>
        	    stream.println(quote(x) + "->" + quote(field).replaceAll("\"","") + " = " + quote(value) + ";")
	        case DynamicRecordForEach(x, init, block) => 
    	        stream.println("val x" + sym.toString.replace("Sym(","").replace(")","") + " = {")
        	    stream.println("\tvar " + quote(init) + "=" + quote(x))
            	stream.println("\twhile (" + quote(init) + " != null) {")
	            emitBlock(block)
    	        stream.println("\t\t" + quote(init) + "=" + quote(init) + ".next")
        	    stream.println("\t}")
            	stream.println("}")              
			case _ => super.emitNode(sym, rhs)
		}
	}
    
    override def emitDataStructures(out: PrintWriter) {
        DynamicRecordsMap.registeredDynamicRecords.foreach(
            rec => writeRecord(out, rec._1, rec._2)
        )
        //DynamicRecordsMap.registeredDynamicRecords.clear
    }

	def writeRecord(out: PrintWriter, className: String, attrs: List[(String, Class[_])]) {
		out.print("struct " + className + " {\n")
		for ((p1,p2) <- attrs) out.print(remapInternal(scala.reflect.ClassManifestFactory.fromClass(p2).toString) + " " + p1 + ";\n")
        out.println("struct " + className + "* next;")
		out.print("};\n")
        out.flush
    }

	def newDynamicRecordType(name: String, attrs: List[(String, Class[_])]) {
        if (DynamicRecordsMap.registeredDynamicRecords.get(name) == None) {
            // Register for first use
            DynamicRecordsMap.registeredDynamicRecords += (name -> attrs)
			val filename = DynamicRecordsMap.dataPath + name + ".c"
            val writer = new PrintWriter(new java.io.File(filename))
            writeRecord(writer, name, attrs)
        }
    }
}

// HASHMAP
trait DynamicRecordHashMap extends Base with HashMapOps with Variables {
  implicit def dRecHashMapToRepHashMapOps[K:Manifest,V:Manifest](m: HashMap[K,V]) = new dynamicRecordHashMapOpsCls[K,V](unit(m))
  implicit def dRecrepHashMapToHashMapOps[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]]) = new dynamicRecordHashMapOpsCls[K,V](m)
  implicit def dRecvarrepHashMapToHashMapOps[K:Manifest,V:Manifest](m: Var[HashMap[K,V]]) = new dynamicRecordHashMapOpsCls[K,V](readVar(m))

  class dynamicRecordHashMapOpsCls[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]]) {
    def apply(k: Rep[K])(implicit pos: SourceContext) = hashmap_apply(m, k)
    def size(implicit pos: SourceContext) = hashmap_size(m)
    def removeHead(implicit pos:SourceContext) = hashmap_removehead(m)
    def getOrElseUpdate(k: Rep[K], v: => Rep[V], h: Rep[DynamicRecord] => Rep[Int] = null, e: (Rep[DynamicRecord],Rep[DynamicRecord])=>Rep[Boolean]=null)(implicit pos: SourceContext) = hashmap_getorelseupdate[K,V](m,k,v,h,e)
    def mkString(delimiter: Rep[String]) = hashmap_mkString(m, delimiter)
  }

  override def hashmap_new[K:Manifest,V:Manifest](specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) : Rep[HashMap[K,V]]
  override def hashmap_apply[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[V]
  override def hashmap_size[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int]
  override def hashmap_removehead[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[(K,V)]
  def hashmap_getorelseupdate[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: => Rep[V], h: Rep[DynamicRecord] => Rep[Int] = null, e: (Rep[DynamicRecord],Rep[DynamicRecord])=>Rep[Boolean] = null)(implicit pos: SourceContext): Rep[V]
  override def hashmap_mkString[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]], v: Rep[String])(implicit pos: SourceContext): Rep[String]
}

trait DynamicRecordHashMapExp extends DynamicRecordHashMap with EffectExp with HashMapOpsExp with DynamicRecordExp {
  abstract class DynamicRecordHashMapDef[K:Manifest,V:Manifest,R:Manifest] extends Def[R] {
    val mK = manifest[K]
    val mV = manifest[V]
  }
  case class DynamicRecordHashMapNew[K:Manifest,V:Manifest](specializedKey: String = "", specializedValue: String ="") extends DynamicRecordHashMapDef[K,V,HashMap[K,V]] 
  case class DynamicRecordHashMapApply[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K]) extends DynamicRecordHashMapDef[K,V,V]
  case class DynamicRecordHashMapSize[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends DynamicRecordHashMapDef[K,V,Int]
  case class DynamicRecordHashMapRemoveHead[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]]) extends DynamicRecordHashMapDef[K,V,(K,V)]
  case class DynamicRecordHashMapGetOrElseUpdate[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K], v: Block[V], h: Block[Int], e: Block[Boolean], d: Sym[DynamicRecord]) extends DynamicRecordHashMapDef[K,V,V]
  case class DynamicRecordHashMapMkString[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], v:Rep[String]) extends DynamicRecordHashMapDef[K,V,String]

  override def hashmap_new[K:Manifest,V:Manifest](specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) = reflectMutable(DynamicRecordHashMapNew[K,V](specializedKey, specializedValue))
  override def hashmap_apply[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]], k: Exp[K])(implicit pos: SourceContext) = DynamicRecordHashMapApply(m,k)
  override def hashmap_size[K:Manifest,V:Manifest](m: Exp[HashMap[K,V]])(implicit pos: SourceContext) = DynamicRecordHashMapSize(m)
  override def hashmap_removehead[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext) = reflectWrite(m)(DynamicRecordHashMapRemoveHead(m))
  def hashmap_getorelseupdate[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: => Exp[V], h: Exp[DynamicRecord] => Exp[Int] = null, e: (Exp[DynamicRecord],Exp[DynamicRecord])=>Exp[Boolean] = null)(implicit pos: SourceContext) = { 
    val b = reifyEffects(v)
    val f = reifyEffects(h(k.asInstanceOf[Rep[DynamicRecord]]))
    val ff = fresh[DynamicRecord]
    val g = reifyEffects(e(k.asInstanceOf[Rep[DynamicRecord]],ff))
    reflectWrite(m)(DynamicRecordHashMapGetOrElseUpdate(m,k,b,f,g,ff))
  }
  override def hashmap_mkString[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]], v: Rep[String])(implicit pos: SourceContext) = reflectEffect(DynamicRecordHashMapMkString(m, v))
  
  //override def syms(p: Any): List[Sym[Any]] = p match {
  //  case HashMapGetOrElseUpdate(m, k, v,h,e) => syms(m):::syms(v)
  //  case _ => super.syms(p)
  //}

  override def boundSyms(p: Any): List[Sym[Any]] = p match {
    case DynamicRecordHashMapGetOrElseUpdate(m, k, v,h,e,d) => effectSyms(h) ::: effectSyms(v) ::: effectSyms(e)
    case _ => super.boundSyms(p)
  }

  //override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
  //  case HashMapGetOrElseUpdate(m, k, v,h,e) => freqNormal(m) ::: freqHot(v)
  //  case _ => super.symsFreq(e)
  //}
}

trait ScalaGenDynamicRecordHashMap extends ScalaGenBase with GenericNestedCodegen with ScalaGenEffect {
  val IR: DynamicRecordHashMapExp
  import IR._
 
  def quoteSizeSymbol(m: Exp[_]): String = {
	    val sizeSymbol = findInitSymbol(m)
		"__" + quote(sizeSymbol) + "Size"
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@DynamicRecordHashMapNew(spkey, spvalue) => {
        val key = if (spkey != "") spkey else remap(m.mK)
        val value = if (spvalue != "") spvalue else remap(m.mV)
        stream.println("var " + quote(sym) + " = new Array[" + value + "](16)")
        stream.println("var __" + quote(sym) + "Size = 0")
    }
    case DynamicRecordHashMapSize(m) => emitValDef(sym, quoteSizeSymbol(m))
    case DynamicRecordHashMapRemoveHead(m) => {
        stream.println("val " + quote(sym) + "= {")
        stream.println("var __idx = 0")
        stream.println("var __elem = " + quote(m) + "(__idx)")
        stream.println("while (__elem == null && __idx < " + quote(m) + ".length) {")
        stream.println("__idx = __idx + 1")
        stream.println("__elem = " + quote(m) + "(__idx)")
        stream.println("}")
        stream.println(quote(m) + "(__idx) = __elem.next")
        stream.println(quoteSizeSymbol(m) + " -= 1")
        stream.println("(__elem, __elem)")
        stream.println("}")
    }
    case DynamicRecordHashMapGetOrElseUpdate(m,k,v,h,e,d)  => {
        stream.println("val ones = " + quote(m) + ".length - 1")
        stream.println("var bc = ones")
        stream.println("bc = bc - ((bc >>> 1) & 0x55555555);")
        stream.println("bc = (bc & 0x33333333) + ((bc >>> 2) & 0x33333333);")
        stream.println("bc = (bc + (bc >>> 4)) & 0x0f0f0f0f;")
        stream.println("bc = bc + (bc >>> 8);")
        stream.println("bc = bc + (bc >>> 16);")
        stream.println("bc = bc & 0x3f;")
        emitBlock(h)
        stream.println("var hc = " + quote(getBlockResult(h)) + " * 0x9e3775cd")
        stream.println("hc = ((hc >>> 24)           ) |")
        stream.println("     ((hc >>   8) &   0xFF00) |")
        stream.println("     ((hc <<   8) & 0xFF0000) |")
        stream.println("     ((hc << 24));")
        stream.println("hc = hc * 0x9e3775cd")
        stream.println("val rotation = bc % 32")
        stream.println("val improved = (hc >>> rotation) | (hc << (32 - rotation))")
        stream.println("val h = (improved >> (32 - bc)) & ones")
        stream.println("var e = " + quote(m) + "(h)")
        stream.println("while (e != null && !{")
        val savedStream = stream
        val newSource = new StringWriter()
        stream = new PrintWriter(newSource)
        emitBlock(e)
        stream = savedStream
        val outStream = newSource.toString.replaceAll(quote(d), "e")
        stream.println(outStream)
        emitBlockResult(e)
        stream.println("}) e = e.next")
        stream.println("var " + quote(sym) + " = e")
        stream.println("if (e eq null) {")
        emitBlock(v)
        stream.println(quote(sym) + " = " + quote(getBlockResult(v)))
        stream.println(quote(sym) + ".next = " + quote(m) + "(h)")
        stream.println(quote(m) + "(h) = " + quote(sym))
        stream.println(quoteSizeSymbol(m) + " = " + quoteSizeSymbol(m) + " + 1")
        stream.println("}")
    }
    case _ => super.emitNode(sym, rhs)
  }
}*/
