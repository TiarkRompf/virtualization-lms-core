package scala.lms
package internal

/* Defines Cuda specific device transfer functions */
trait CudaDeviceTransfer extends AbstractDeviceTransfer {
  this: CudaCodegen =>

  val IR: Expressions
  import IR._


  def emitSendSlave(tp:TypeRep[Any]): (String,String) = {
    if (isPrimitiveType(tp)) {
      val out = new StringBuilder
      val signature = "%s sendCuda_%s(%s sym)".format(remap(tp),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\treturn sym;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if (tp.erasure == classOf[List[Any]]) {
      val out = new StringBuilder
      val signature = "%s *sendCuda_%s(Host%s *sym)".format(remap(tp),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\t%s *sym_dev = new %s();\n".format(remap(tp),remap(tp)))
      out.append("\treturn sym_dev;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + tp.toString)
    }
  }

  def emitRecvSlave(tp:TypeRep[Any]): (String,String) = {
    if (isPrimitiveType(tp)) {
      val out = new StringBuilder
      val signature = "%s recvCuda_%s(%s *sym_dev)".format(remap(tp),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\t%s *hostPtr;\n".format(remap(tp)))
      out.append("\tDeliteCudaMallocHost((void**)&hostPtr,sizeof(%s));\n".format(remap(tp)))
      out.append("\tDeliteCudaMemcpyDtoHAsync(hostPtr, sym_dev, sizeof(%s));\n".format(remap(tp)))
      out.append("\treturn *hostPtr;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if (tp.erasure == classOf[List[Any]]) {
      val out = new StringBuilder
      val signature = "Host%s *recvCuda_%s(%s *sym_dev)".format(remap(tp),mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\tHost%s *sym = new Host%s();\n".format(remap(tp),remap(tp)))
      out.append("\treturn sym;\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + tp.toString)
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
  def emitSendUpdateSlave(tp:TypeRep[Any]): (String,String) = {
    if(isPrimitiveType(tp)) {
      val out = new StringBuilder
      val signature = "void sendUpdateCuda_%s(%s sym)".format(mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if (tp.erasure == classOf[List[Any]]) {
      val out = new StringBuilder
      val signature = "void sendUpdateCuda_%s(Host%s *sym, %s *sym_dev)".format(mangledName(remap(tp)),remap(tp),remap(tp))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + tp.toString)
    }
  }

  def emitRecvUpdateSlave(tp:TypeRep[Any]): (String,String) = {
    if(isPrimitiveType(tp)) {
      val out = new StringBuilder
      val signature = "void recvUpdateCuda_%s(%s sym)".format(mangledName(remap(tp)),remap(tp))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else if (tp.erasure == classOf[List[Any]]) {
      val out = new StringBuilder
      val signature = "void recvUpdateCuda_%s(Host%s *sym, %s *sym_dev)".format(mangledName(remap(tp)),remap(tp),remap(tp))
      out.append(signature + " {\n")
      out.append("\tassert(false);\n")
      out.append("}\n")
      (signature+";\n", out.toString)
    }
    else {
      throw new GenerationFailedException("CudaDeviceTransfer: Unknown type " + tp.toString)
    }
  }

}
