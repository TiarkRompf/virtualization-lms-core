package scala.virtualization.lms
package common

import java.io._
import reflect.ClassManifest
import scala.collection.mutable.HashMap
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util._
import scala.reflect.SourceContext

object DynamicRecordsMap extends Serializable {
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
 
    def writeRecord(out: PrintWriter, className: String, attrs: List[(String, Class[_])]) {
        out.print("class " + className + " extends scala.virtualization.lms.common.DynamicRecordExp {\n")
        for ((p1,p2) <- attrs) {
             val str = {
                 if (ClassManifest.fromClass(p2) == classManifest[Array[Byte]])
                    "Array[Byte]"
                 else 
                    p2.toString.replaceAll("class ","").replaceAll("int", "scala.Int").replaceAll("double","scala.Double").replaceAll("char", "Char").replaceAll("long", "scala.Long")
             }
             out.print("var " + p1 + ": " + str + " = null.asInstanceOf[" + str + "];\n")
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
        if (registeredDynamicRecords.get(name) == None) {
            // Register for first use
            registeredDynamicRecords += (name -> attrs)
            // Write to file (for persistence)
            val writer = new PrintWriter(new java.io.File(dataPath + name + ".scala"))
            writeRecord(writer, name, attrs)
            writer.close()
        }
    }
}

trait DynamicRecord extends Base with Serializable with VariablesExp {
	class DynamicRecordOps(x: Rep[DynamicRecord]) {
        def get(field: Rep[Any]) = dynamicRecordGet(x, field)
		def set(field: Rep[Any], value: Rep[Any]) = dynamicRecordSet(x, field, value)
        def foreach(f: Rep[DynamicRecordExp] => Rep[Unit]) = dynamicRecordForEach(x,f)
    }
	implicit def varDynamicType2dynamicRecordOps(x: Var[DynamicRecord]) = new DynamicRecordOps(readVar(x))
	implicit def varDynamicType2dynamicRecord(x: Var[DynamicRecord]) = readVar(x)
	implicit def dynamicRecord2dynamicRecordOps(x: Rep[DynamicRecord]) = new DynamicRecordOps(x)
	implicit def dynamicRecord2RepdynamicRecordOps(x: DynamicRecord) = new DynamicRecordOps(unit(x))

    def newDynamicRecord(name: String, reuse: Boolean = false): Rep[DynamicRecordExp]
	def dynamicRecordGet(x: Rep[DynamicRecord], field: Rep[Any]): Rep[Any]
	def dynamicRecordSet(x: Rep[DynamicRecord], field: Rep[Any], value: Rep[Any]): Rep[Unit]
    def dynamicRecordForEach(x: Rep[DynamicRecord], f: Rep[DynamicRecordExp] => Rep[Unit]): Rep[Unit]
}

trait DynamicRecordExp extends DynamicRecord with BaseExp with EffectExp {
    case class NewDynamicRecord(n: String) extends Def[DynamicRecordExp]
	case class DynamicRecordGet(x: Rep[DynamicRecord], field: Rep[Any]) extends Def[Any]
	case class DynamicRecordSet(x: Rep[DynamicRecord], field: Rep[Any], value: Rep[Any]) extends Def[Unit]
    case class DynamicRecordForEach(l: Rep[DynamicRecord], x: Sym[DynamicRecordExp], block: Block[Unit]) extends Def[Unit]

    def newDynamicRecord(name: String, reuse: Boolean = false) = 
        if (reuse) NewDynamicRecord(name) else reflectEffect(NewDynamicRecord(name))
	def dynamicRecordGet(x: Rep[DynamicRecord], field: Rep[Any]) = reflectEffect(DynamicRecordGet(x, field))
	def dynamicRecordSet(x: Rep[DynamicRecord], field: Rep[Any], value: Rep[Any]) = reflectEffect(DynamicRecordSet(x,field,value))
    def dynamicRecordForEach(x: Rep[DynamicRecord], f: Exp[DynamicRecordExp] => Exp[Unit])={
        val a = fresh[DynamicRecordExp]
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
            case NewDynamicRecord(x) => emitValDef(sym, "new " + x + "()")
		    case DynamicRecordGet(x, field) => emitValDef(sym, quote(x) + "." + quote(field).replaceAll("\"",""))
		    case DynamicRecordSet(x, field, value) =>
                 stream.println(quote(x) + "." + quote(field).replaceAll("\"","") + " = " + quote(value))
            case DynamicRecordForEach(x, init, block) => 
                stream.println("val " + quote(sym) + " = {")
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
            rec => DynamicRecordsMap.writeRecord(out, rec._1, rec._2)
        )
        //DynamicRecordsMap.registeredDynamicRecords.clear
    }
}
