package scala.virtualization.lms.internal

trait CppHostTransfer extends AbstractHostTransfer with CLikeCodegen {

  val IR: Expressions
  import IR._

  override def emitSend(sym: Sym[Any], host: Hosts.Value): String = {
    if (host == Hosts.JVM) {
      if (isPrimitiveType(sym.tp)) {
        val out = new StringBuilder
        out.append("j%s sendCPPtoJVM_%s(JNIEnv *env, %s %s) {\n".format(remapToJNI(sym.tp).toLowerCase,quote(sym),remap(sym.tp),quote(sym)))
        out.append("\treturn (j%s)%s;\n".format(remapToJNI(sym.tp).toLowerCase,quote(sym)))
        out.append("}\n")
        out.toString
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + sym.tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def emitRecv(sym: Sym[Any], host: Hosts.Value): String = {
    if (host == Hosts.JVM) {
      if (isPrimitiveType(sym.tp)) {
        val out = new StringBuilder
        out.append("%s recvCPPfromJVM_%s(JNIEnv *env, j%s %s) {\n".format(remap(sym.tp),quote(sym),remapToJNI(sym.tp).toLowerCase,quote(sym)))
        out.append("\treturn (%s)%s;\n".format(remap(sym.tp),quote(sym)))
        out.append("}\n")
        out.toString
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + sym.tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def emitUpdated(sym: Sym[Any], host: Hosts.Value): String = {
    if (host == Hosts.JVM) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
  }

  def emitUpdate(sym: Sym[Any], host: Hosts.Value): String = {
    if (host == Hosts.JVM) {
      if(isPrimitiveType(sym.tp)) {
        val out = new StringBuilder
        out.append("void updateCPPtoJVM_%s(JNIEnv *env, %s %s) {\n".format(quote(sym),remap(sym.tp),quote(sym)))
        //out.append("\treturn (j%s)%s;\n".format(remapToJNI(sym.tp).toLowerCase,quote(sym)))
        out.append("}\n")
        out.toString
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + sym.tp.toString)
      }
    }
    else if (host == Hosts.CPP) {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown host " + host.toString)
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
