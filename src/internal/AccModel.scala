package scala.virtualization.lms.internal

trait AbstractHostTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSend(tp: Manifest[_], peer: Targets.Value): (String,String)
  def emitRecv(tp: Manifest[_], peer: Targets.Value): (String,String)
  def emitSendView(tp: Manifest[_], peer: Targets.Value): (String,String)
  def emitRecvView(tp: Manifest[_], peer: Targets.Value): (String,String)
  def emitSendUpdate(tp: Manifest[_], peer: Targets.Value): (String,String)
  def emitRecvUpdate(tp: Manifest[_], peer: Targets.Value): (String,String)
  def emitMakeManifest(tp: Manifest[_]): (String,String)
}

trait AbstractDeviceTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSendSlave(tp: Manifest[_]) : (String,String)
  def emitRecvSlave(tp: Manifest[_]) : (String,String)
  //def emitSendViewSlave(tp: Manifest[_]) : (String,String)
  //def emitRecvViewSlave(tp: Manifest[_]) : (String,String)
  def emitSendUpdateSlave(tp: Manifest[_]) : (String,String)
  def emitRecvUpdateSlave(tp: Manifest[_]) : (String,String)

  //def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit
}

object Targets extends Enumeration {
  
  //TODO: Get rid of JVM target, or make an hierarchy
  val JVM = Value("jvm")
  val Scala = Value("scala")
  val Cpp = Value("cpp")
  val Cuda = Value("cuda")
  val OpenCL = Value("opencl")
  val Dot = Value("dot")
  val MaxJ = Value("maxj")
  val Chisel = Value("chisel")
  
  def apply(s: String): Value = s.toLowerCase() match {
    case "jvm" => JVM
    case "scala" => Scala
    case "cpp" => Cpp
    case "cuda" => Cuda
    case "opencl" => OpenCL
    case "dot" => Dot
    case "maxj" => MaxJ 
    case "chisel" => Chisel
    case _ => throw new IllegalArgumentException("unsupported target: " + s)
  }

  def getHostTarget(target: Value): Targets.Value = {
    target match {
      case Targets.Scala => Targets.Scala
      case Targets.Cpp => Targets.Cpp
      case Targets.Cuda => Targets.Cpp
      case Targets.OpenCL => Targets.Cpp
      case Targets.Dot => Targets.Dot
      case Targets.MaxJ => Targets.MaxJ
      case Targets.Chisel => Targets.Chisel
      case _ => throw new IllegalArgumentException("Cannot find a host target for target " + target)
    }
  }

  implicit def targettostring(target: Targets.Value): String = target.toString
}
