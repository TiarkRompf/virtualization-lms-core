package scala.virtualization.lms.internal

trait CppHostTransfer extends AbstractHostTransfer {
  this: CLikeCodegen =>

  val IR: Expressions
  import IR._

  def emitSend(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "%s sendCPPtoJVM_%s(JNIEnv *env, %s sym)".format(JNIType(tp),mangledName(remap(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\treturn (%s)sym;\n".format(JNIType(tp)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure == classOf[List[Any]]) {
        val out = new StringBuilder
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, Host%s *sym)".format(mangledName(remap(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn NULL;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def emitRecv(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "%s recvCPPfromJVM_%s(JNIEnv *env, %s sym)".format(remap(tp),mangledName(remap(tp)),JNIType(tp))
        out.append(signature + " {\n")
        out.append("\treturn (%s)sym;\n".format(remap(tp)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure == classOf[List[Any]]) {
        val out = new StringBuilder
        val signature = "Host%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remap(tp),mangledName(remap(tp)))
        out.append(signature + " {\n")
        out.append("\tHost%s *sym = new Host%s();\n".format(remap(tp),remap(tp)))
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def emitSendView(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "%s sendViewCPPtoJVM_%s(JNIEnv *env, %s sym)".format(JNIType(tp),mangledName(remap(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn (%s)sym;\n".format(JNIType(tp)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure == classOf[List[Any]]) {
        val out = new StringBuilder
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, Host%s *sym)".format(mangledName(remap(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn NULL;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def emitRecvView(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if (isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "%s recvViewCPPfromJVM_%s(JNIEnv *env, %s sym)".format(remap(tp),mangledName(remap(tp)),JNIType(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn (%s)sym;\n".format(remap(tp)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure == classOf[List[Any]]) {
        val out = new StringBuilder
        val signature = "Host%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject *obj)".format(remap(tp),mangledName(remap(tp)))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn NULL;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def emitSendUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if(isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, %s sym)".format(mangledName(remap(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure == classOf[List[Any]]) {
        val out = new StringBuilder
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, Host%s *sym)".format(mangledName(remap(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def emitRecvUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String) = {
    if (host == Hosts.JVM) {
      if(isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, %s sym)".format(mangledName(remap(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (tp.erasure == classOf[List[Any]]) {
        val out = new StringBuilder
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, Host%s *sym)".format(mangledName(remap(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def JNIType[A](m: Manifest[A]) : String = {
    remap(m) match {
      case "bool" => "jboolean"
      case "char" => "jbyte"
      case "CHAR" => "jchar"
      case "short" => "jshort"
      case "int" => "jint"
      case "long" => "jlong"
      case "float" => "jfloat"
      case "double" => "jdouble"
      case _ => "jobject"//all other types are objects
    }
  }

  def remapToJNI[A](m: Manifest[A]) : String = {
    remap(m) match {
      case "bool" => "Boolean"
      case "char" => "Byte"
      case "CHAR" => "Char"
      case "short" => "Short"
      case "int" => "Int"
      case "long" => "Long"
      case "float" => "Float"
      case "double" => "Double"
      case _ => "Object"
    }
  }

  def JNITypeDescriptor[A](m: Manifest[A]) : String = JNITypeDescriptor(m.toString)
  def JNITypeDescriptor(tp: String): String = tp match {
    case "Boolean" => "Z"
    case "Byte" => "B"
    case "Char" => "C"
    case "Short" => "S"
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case array if array.startsWith("Array[") => "[" + JNITypeDescriptor(array.slice(6,array.length-1))
    case _ => { //all other types are objects
      var objectType = tp.replace('.','/')
      if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
      "L"+objectType+";" //'L' + fully qualified type + ';'
    }
    //case _ => throw new GenerationFailedException("Undefined JNI type")
  }

}
