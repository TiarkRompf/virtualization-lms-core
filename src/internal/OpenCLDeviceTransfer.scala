package scala.virtualization.lms.internal

/* Defines OpenCL specific device transfer functions */
trait OpenCLDeviceTransfer extends AbstractDeviceTransfer {
  this: OpenCLCodegen =>

  val IR: Expressions
  import IR._

  def emitSendSlave(tp: Manifest[Any]): (String,String) = {
    if (isPrimitiveType(tp)) {
      val out = new StringBuilder
      val signature = "%s sendOpenCL_%s(%s sym)".format(remap(tp),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\treturn sym;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("OpenCLDeviceTransfer: Unknown type " + tp.toString)
    }
  }

  def emitRecvSlave(tp: Manifest[Any]): (String,String) = {
    if (isPrimitiveType(tp)) {
      val out = new StringBuilder
      val signature = "%s recvOpenCL_%s(%s sym)".format(remap(tp),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\treturn sym;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("OpenCLDeviceTransfer: Unknown type " + tp.toString)
    }
  }
/*
  def emitSendViewSlave(sym: Sym[Any]): (String,String) = {
    if (isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      val signature = "%s sendViewOpenCL_%s(%s %s)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("\treturn %s;\n".format(quote(sym)))
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("OpenCLDeviceTransfer: Unknown type " + sym.tp.toString)
    }
  }

  def emitRecvViewSlave(sym: Sym[Any]): (String,String) = {
    if (isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      val signature = "%s recvViewOpenCL_%s(%s %s)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("\treturn %s;\n".format(quote(sym)))
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("OpenCLDeviceTransfer: Unknown type " + sym.tp.toString)
    }
  }
*/
  def emitSendUpdateSlave(tp: Manifest[Any]): (String,String) = {
    if(isPrimitiveType(tp)) {
      val out = new StringBuilder
      val signature = "void sendUpdateOpenCL_%s(%s sym)".format(mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("OpenCLDeviceTransfer: Unknown type " + tp.toString)
    }
  }

  def emitRecvUpdateSlave(tp: Manifest[Any]): (String,String) = {
    if(isPrimitiveType(tp)) {
      val out = new StringBuilder
      val signature = "void recvUpdateOpenCL_%s(%s sym)".format(mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("OpenCLDeviceTransfer: Unknown type " + tp.toString)
    }
  }
}
