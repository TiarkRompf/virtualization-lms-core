package scala.lms.internal

trait CppHostTransfer extends AbstractHostTransfer {
  this: CLikeCodegen =>

  val IR: Expressions
  import IR._

  // NOTE: strings are in general treated as primitive types to avoid the memory management,
  //       but for transfer functions strings must be treated separately from primitive types

  def emitSend(tp: Typ[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (remap(tp) == "string") {
        val out = new StringBuilder
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s sym)".format(mangledName(remapHost(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("return env->NewStringUTF(sym.c_str());\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "%s sendCPPtoJVM_%s(JNIEnv *env, %s sym)".format(JNIType(tp),mangledName(remapHost(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\treturn (%s)sym;\n".format(JNIType(tp)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isListType(tp)) {
        val out = new StringBuilder
        val signature = "jobject sendCPPtoJVM_%s(JNIEnv *env, %s *sym)".format(mangledName(remapHost(tp)),remap(tp))
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
    else if (peer == Targets.Cpp) {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
  }

  def emitRecv(tp: Typ[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (remap(tp) == "string") {
        val out = new StringBuilder
        val signature = "%s recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        out.append("\tconst char *str = env->GetStringUTFChars((jstring)obj,NULL);\n")
        //TODO: check if this copy is necessary
        out.append("\tchar *str2 = (char *)malloc((1+strlen(str))*sizeof(char));\n")
        out.append("\tstrcpy(str2,str);\n")
        out.append("\tstring sym(str2);\n")
        out.append("\tenv->ReleaseStringUTFChars((jstring)obj,str);")
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)        
      }
      else if (isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "%s recvCPPfromJVM_%s(JNIEnv *env, %s sym)".format(remap(tp),mangledName(remapHost(tp)),JNIType(tp))
        out.append(signature + " {\n")
        out.append("\treturn (%s)sym;\n".format(remap(tp)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isListType(tp)) {
        val out = new StringBuilder
        val signature = "%s *recvCPPfromJVM_%s(JNIEnv *env, jobject obj)".format(remapHost(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        out.append("\t%s *sym = new %s();\n".format(remapHost(tp),remapHost(tp)))
        out.append("\treturn sym;\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (peer == Targets.Cpp) {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
  }

  def emitSendView(tp: Typ[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (remap(tp) == "string") {
        val out = new StringBuilder
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn NULL;\n")
        out.append("}\n")
        (signature+";\n", out.toString)        
      }
      else if (isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "%s sendViewCPPtoJVM_%s(JNIEnv *env, %s sym)".format(JNIType(tp),mangledName(remapHost(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn (%s)sym;\n".format(JNIType(tp)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isListType(tp)) {
        val out = new StringBuilder
        val signature = "jobject sendViewCPPtoJVM_%s(JNIEnv *env, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
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
    else if (peer == Targets.Cpp) {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
  }

  def emitRecvView(tp: Typ[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (remap(tp) == "string") {
        val out = new StringBuilder
        val signature = "%s recvViewCPPfromJVM_%s(JNIEnv *env, jobject *obj)".format(remapHost(tp),mangledName(remapHost(tp)))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn NULL;\n")
        out.append("}\n")
        (signature+";\n", out.toString)        
      }
      else if (isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "%s recvViewCPPfromJVM_%s(JNIEnv *env, %s sym)".format(remap(tp),mangledName(remapHost(tp)),JNIType(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("\treturn (%s)sym;\n".format(remap(tp)))
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isListType(tp)) {
        val out = new StringBuilder
        val signature = "%s *recvViewCPPfromJVM_%s(JNIEnv *env, jobject *obj)".format(remapHost(tp),mangledName(remapHost(tp)))
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
    else if (peer == Targets.Cpp) {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
  }

  def emitSendUpdate(tp: Typ[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if(isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, %s sym)".format(mangledName(remapHost(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isListType(tp)) {
        val out = new StringBuilder
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (remap(tp) == "string") {
        val out = new StringBuilder
        val signature = "void sendUpdateCPPtoJVM_%s(JNIEnv *env, jobject obj, %s sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)        
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (peer == Targets.Cpp) {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
  }

  def emitRecvUpdate(tp: Typ[_], peer: Targets.Value): (String,String) = {
    if (peer == Targets.JVM) {
      if (remap(tp) == "string") {
        val out = new StringBuilder
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, %s sym)".format(mangledName(remapHost(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)        
      }
      else if(isPrimitiveType(tp)) {
        val out = new StringBuilder
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, %s sym)".format(mangledName(remapHost(tp)),remap(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else if (isListType(tp)) {
        val out = new StringBuilder
        val signature = "void recvUpdateCPPfromJVM_%s(JNIEnv *env, jobject obj, %s *sym)".format(mangledName(remapHost(tp)),remapHost(tp))
        out.append(signature + " {\n")
        out.append("\tassert(false);\n")
        out.append("}\n")
        (signature+";\n", out.toString)
      }
      else {
        throw new GenerationFailedException("CppHostTransfer: Unknown type " + tp.toString)
      }
    }
    else if (peer == Targets.Cpp) {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
    else {
      throw new GenerationFailedException("CppHostTransfer: Unknown peer " + peer.toString)
    }
  }

  def JNIType[A](m: Typ[A]) : String = {
    remap(m) match {
      case "bool" => "jboolean"
      case "int8_t" => "jbyte"
      case "uint16_t" => "jchar"
      case "int16_t" => "jshort"
      case "int32_t" => "jint"
      case "int64_t" => "jlong"
      case "float" => "jfloat"
      case "double" => "jdouble"
      case _ => "jobject"//all other types are objects
    }
  }

  def remapToJNI[A](m: Typ[A]) : String = {
    remap(m) match {
      case "bool" => "Boolean"
      case "int8_t" => "Byte"
      case "uint16_t" => "Char"
      case "int16_t" => "Short"
      case "int32_t" => "Int"
      case "int64_t" => "Long"
      case "float" => "Float"
      case "double" => "Double"
      case _ => "Object"
    }
  }

  def JNITypeDescriptor[A](m: Typ[A]) : String = JNITypeDescriptor(m.toString)
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
