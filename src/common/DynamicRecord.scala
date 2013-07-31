package scala.virtualization.lms
package common

import java.io._
import reflect.ClassManifest
import scala.collection.mutable.HashMap
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util._

object DynamicRecordsMap extends Serializable {
    var mapInitialized: Boolean = false;
    var dataPath: String = null;
    var registeredDynamicRecords: HashMap[String, List[(String, Class[_])]] = null
    
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
                    p2.toString.replaceAll("class ","").replaceAll("int", "scala.Int").replaceAll("double","scala.Double").replaceAll("char", "Char")
             }
             out.print("var " + p1 + ": " + str + " = null.asInstanceOf[" + str + "];\n")
        }
        // Custom toString function
        out.print("override def toString() = {\n\"\"+")
        out.print( (for ((p1,p2) <- attrs) yield if (ClassManifest.fromClass(p2) == classManifest[Array[Byte]]) "new String(" + p1 + ")" else p1).mkString("+\"|\"+") )
        out.print("}\n");
        // Custom serialization/deserialization routines
        out.println("@throws(classOf[java.io.IOException])")
        out.println("private def writeObject(out: java.io.ObjectOutputStream): Unit = {")
        for ((p1,p2) <- attrs) {
            ClassManifest.fromClass(p2) match {
                case m if m <:< classManifest[scala.Int]        => out.println("out.writeInt(" + p1 + ")")
                case m if m <:< classManifest[scala.Double]     => out.println("out.writeDouble(" + p1 + ")")
                case m if m <:< classManifest[scala.Char]       => out.println("out.writeChar(" + p1 + ")")
                case m if m <:< classManifest[java.lang.String] => out.println("out.writeUTF(" + p1 + ")")
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
                case m if m <:< classManifest[java.lang.String] => out.println(p1 + " = in.readUTF()")
                case m if m == classManifest[Array[Byte]]       => out.println(p1 + " = in.readObject().asInstanceOf[Array[Byte]]")
                case _ => out.println(p1 + " = in.readObject().asInstanceOf[" + p2.toString.replaceAll("class ","") + "]")
            }
        }
        out.println("}")
        out.println("}") // End of class
        out.flush
    }

	def newDynamicRecordType(name: String, attrs: List[(String, Class[_])]) {
        // Register for first use
        registeredDynamicRecords += (name -> attrs)
        // Write to file (for persistence)
        val writer = new PrintWriter(new java.io.File(dataPath + name + ".scala"))
        writeRecord(writer, name, attrs)
        writer.close()
    }
}

object DynamicRecordGenericReflectionFields {
    val genericDynamicRecord = { 
        trait Optimized extends ScalaOpsPkg with DynamicRecord with ScalaCompile {
            def __getGenericRecord() = newDynamicRecord("K2DBGenericDynamicRecord")
        }
        // Now compile method
        val code = new Optimized with ScalaOpsPkgExp with DynamicRecordExp { 
            self => val codegen = new ScalaCodeGenPkg with ScalaGenDynamicRecord {
                val IR: self.type = self 
            }
        }
        DynamicRecordsMap.registeredDynamicRecords += ("K2DBGenericDynamicRecord" -> 
                                     List(("K2DBGenericDynamicRecordField",classOf[java.lang.String])))
        code.compile0(code.__getGenericRecord)
    }
    val genericNames = genericDynamicRecord().getClass.getDeclaredFields.map(x => x.getName)
    val genericTypes = genericDynamicRecord().getClass.getDeclaredFields.map(x => x.getType)
}

trait DynamicRecord extends Base with Serializable {
	class DynamicRecordOps(x: Rep[DynamicRecord]) {
        def get(field: Rep[String]) = dynamicTypeGet(x, field)
		def set(field: Rep[String], value: Rep[Any]) = dynamicTypeSet(x, field, value)	
	}
	implicit def dynamicType2dynamicTypeOps(x: Rep[DynamicRecord]) = new DynamicRecordOps(x)

    def newDynamicRecord(name: String): Rep[DynamicRecordExp]
	def dynamicTypeGet(x: Rep[DynamicRecord], field: Rep[String]): Rep[Any]
	def dynamicTypeSet(x: Rep[DynamicRecord], field: Rep[String], value: Rep[Any]): Rep[Unit]
    def getAttrNames() = {
        val fields = this.getClass.getDeclaredFields.filter(x => DynamicRecordGenericReflectionFields.genericNames.contains(x.getName) == false)
        fields.map(x => x.getName)
    }
    def getAttrTypes() = {
        val fields = this.getClass.getDeclaredFields.filter(x => DynamicRecordGenericReflectionFields.genericNames.contains(x.getName) == false)
        fields.map(x => x.getType)
    }
}

trait DynamicRecordExp extends DynamicRecord with BaseExp with EffectExp {
    case class NewDynamicRecord(name: String) extends Def[DynamicRecordExp]
	case class DynamicRecordGet(x: Rep[DynamicRecord], field: Rep[String]) extends Def[Any]
	case class DynamicRecordSet(x: Rep[DynamicRecord], field: Rep[String], value: Rep[Any]) extends Def[Unit]

    def newDynamicRecord(name: String) = reflectEffect(NewDynamicRecord(name))
	def dynamicTypeGet(x: Rep[DynamicRecord], field: Rep[String]) = reflectEffect(DynamicRecordGet(x, field))
	def dynamicTypeSet(x: Rep[DynamicRecord], field: Rep[String], value: Rep[Any]) = reflectEffect(DynamicRecordSet(x, field, value))
}

trait ScalaGenDynamicRecord extends ScalaGenBase  {
	val IR: DynamicRecordExp
	import IR._
  
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) =  { 
        rhs match {
            case NewDynamicRecord(x) => emitValDef(sym, "new " + x + "()")
		    case DynamicRecordGet(x, field) => emitValDef(sym, quote(x) + "." + quote(field).replaceAll("\"",""))
		    case DynamicRecordSet(x, field, value) => stream.println(quote(x) + "." + quote(field).replaceAll("\"","") + "=" + quote(value))
		    case _ => super.emitNode(sym, rhs)
  	    }
    }

    override def emitDataStructures(out: PrintWriter) {
        DynamicRecordsMap.registeredDynamicRecords.foreach(
            rec => DynamicRecordsMap.writeRecord(out, rec._1, rec._2)
        )
        DynamicRecordsMap.registeredDynamicRecords.clear
    }
}
