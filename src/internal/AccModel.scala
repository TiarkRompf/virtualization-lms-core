package scala.virtualization.lms.internal

trait AbstractHostTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSend(tp: Manifest[Any], peer: Targets.Value): (String,String)
  def emitRecv(tp: Manifest[Any], peer: Targets.Value): (String,String)
  def emitSendView(tp: Manifest[Any], peer: Targets.Value): (String,String)
  def emitRecvView(tp: Manifest[Any], peer: Targets.Value): (String,String)
  def emitSendUpdate(tp: Manifest[Any], peer: Targets.Value): (String,String)
  def emitRecvUpdate(tp: Manifest[Any], peer: Targets.Value): (String,String)
}

trait AbstractDeviceTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSendSlave(tp: Manifest[Any]) : (String,String)
  def emitRecvSlave(tp: Manifest[Any]) : (String,String)
  //def emitSendViewSlave(tp: Manifest[Any]) : (String,String)
  //def emitRecvViewSlave(tp: Manifest[Any]) : (String,String)
  def emitSendUpdateSlave(tp: Manifest[Any]) : (String,String)
  def emitRecvUpdateSlave(tp: Manifest[Any]) : (String,String)

  //def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit
}

object Targets extends Enumeration {
  
  //TODO: Get rid of JVM target, or make an hierarchy
  val JVM = Value("jvm")
  val Scala = Value("scala")
  val Cpp = Value("cpp")
  val Cuda = Value("cuda")
  val OpenCL = Value("opencl")
  
  def apply(s: String): Value = s.toLowerCase() match {
    case "jvm" => JVM
    case "scala" => Scala
    case "cpp" => Cpp
    case "cuda" => Cuda
    case "opencl" => OpenCL
    case _ => throw new IllegalArgumentException("unsupported target: " + s)
  }

  def getHostTarget(target: Value): Targets.Value = {
    target match {
      case Targets.Scala => Targets.Scala
      case Targets.Cpp => Targets.Cpp
      case Targets.Cuda => Targets.Cpp
      case Targets.OpenCL => Targets.Cpp
      case _ => throw new IllegalArgumentException("Cannot find a host target for target " + target)
    }
  }

  implicit def targettostring(target: Targets.Value): String = target.toString
}
