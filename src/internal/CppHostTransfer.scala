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
      case _ => throw new GenerationFailedException("GPUGen: Cannot get array creation JNI function for this type " + remap(m))
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
      case _ => throw new GenerationFailedException("GPUGen: Cannot get array creation JNI function for this type " + remap(m))
    }
  }

  def JNITypeDescriptor[A](m: Manifest[A]) : String = m.toString match {
    case "Boolean" => "Z"
    case "Byte" => "B"
    case "Char" => "C"
    case "Short" => "S"
    case "Int" => "I"
    case "Long" => "J"
    case "Float" => "F"
    case "Double" => "D"
    case _ => throw new GenerationFailedException("Undefined JNI type")
  }

}
