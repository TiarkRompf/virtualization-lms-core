package scala.virtualization.lms
package common

import java.io._
import reflect.ClassManifest
import scala.collection.mutable.HashMap

object DynamicRecordsMap extends Serializable {
    var mapInitialized: Boolean = false;
    var dataPath: String = null;
    var registeredDynamicRecords: HashMap[String, List[(String, Class[_])]] = null
    
    private def serializeObject[T](obj: T, file: String) {
		val store = new ObjectOutputStream(new FileOutputStream(new File(file))) 
		store.writeObject(obj) 
		store.close 
	}

  	private def deSerializeObject(file: String) = {
		val in = new ObjectInputStream(new FileInputStream(file)) {
			override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
		    		try { Class.forName(desc.getName, false, getClass.getClassLoader) }
				    catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
  			}
		}
  		val obj = in.readObject() 
		in.close
		obj
	}

    def initialize(rootFolder: String) = {
        if (mapInitialized)
            throw new RuntimeException("DynamicRecordsMap already initialized");
        mapInitialized = true
        dataPath = rootFolder 
        registeredDynamicRecords = {
            if (!(new File(dataPath + "/dynamic_records_file").exists()))
                new HashMap[String, List[(String, Class[_])]]
	        else deSerializeObject(dataPath + "/dynamic_records_file").asInstanceOf[
                HashMap[String, List[(String, Class[_])]]]
        }
    }

    def writeRecord(out: PrintWriter, className: String, attrs: List[(String, Class[_])]) {
        out.print("class " + className + " extends scala.virtualization.lms.common.DynamicRecordExp {\n")
        for ((p1,p2) <- attrs) {
             val str = p2.toString.replaceAll("class ","").replaceAll("int", "scala.Int").replaceAll("double","scala.Double").replaceAll("char", "Char")
             out.print("var " + p1 + ": " + str + " = null.asInstanceOf[" + str + "];\n")
        }
        // Custom toString function
        out.print("override def toString() = {\n")
        out.print( (for ((p1,p2) <- attrs) yield p1).mkString("+\"|\"+") )
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
        for ((p1,p2) <- attrs) {
            ClassManifest.fromClass(p2) match {
                case m if m <:< classManifest[scala.Int]        => out.println(p1 + " = in.readInt()")
                case m if m <:< classManifest[scala.Double]     => out.println(p1 + " = in.readDouble()")
                case m if m <:< classManifest[scala.Char]       => out.println(p1 + " = in.readChar()")
                case m if m <:< classManifest[java.lang.String] => out.println(p1 + " = in.readUTF()")
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
    }
	
    def serialize = serializeObject(registeredDynamicRecords, dataPath+ "/dynamic_records_file")
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
    }
}
