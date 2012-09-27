package scala.virtualization.lms.internal

/* Defines Cuda specific device transfer functions */
trait CudaDeviceTransfer extends AbstractDeviceTransfer with GenericCodegen {

  val IR: Expressions
  import IR._

  def emitSendSlave(sym: Sym[Any]): (String,String) = {
    if (isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      val signature = "%s sendCuda_%s(%s %s)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
      out.append(signature + " {\n")
      out.append("\treturn %s;\n".format(quote(sym)))
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + sym.tp.toString)
    }
  }

  def emitRecvSlave(sym: Sym[Any]): (String,String) = {
    if (isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      val signature = "%s recvCuda_%s(%s %s)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
      out.append(signature + " {\n")
      out.append("\treturn %s;\n".format(quote(sym)))
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + sym.tp.toString)
    }
  }
/*
  def emitSendViewSlave(sym: Sym[Any]): (String,String) = {
    if (isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      val signature = "%s sendViewCuda_%s(%s %s)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("\treturn %s;\n".format(quote(sym)))
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + sym.tp.toString)
    }
  }

  def emitRecvViewSlave(sym: Sym[Any]): (String,String) = {
    if (isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      val signature = "%s recvViewCuda_%s(%s %s)".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(sym))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("\treturn %s;\n".format(quote(sym)))
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + sym.tp.toString)
    }
  }
*/
  def emitSendUpdateSlave(sym: Sym[Any]): (String,String) = {
    if(isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      val signature = "void sendUpdateCuda_%s(%s %s)".format(quote(sym),remap(sym.tp),quote(sym))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + sym.tp.toString)
    }
  }

  def emitRecvUpdateSlave(sym: Sym[Any]): (String,String) = {
    if(isPrimitiveType(sym.tp)) {
      val out = new StringBuilder
      val signature = "void recvUpdateCuda_%s(%s %s)".format(quote(sym),remap(sym.tp),quote(sym))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + sym.tp.toString)
    }
  }

}
