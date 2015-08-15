package scala.lms.internal

trait AbstractHostTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSend(tp: Typ[_], peer: Targets.Value): (String,String)
  def emitRecv(tp: Typ[_], peer: Targets.Value): (String,String)
  def emitSendView(tp: Typ[_], peer: Targets.Value): (String,String)
  def emitRecvView(tp: Typ[_], peer: Targets.Value): (String,String)
  def emitSendUpdate(tp: Typ[_], peer: Targets.Value): (String,String)
  def emitRecvUpdate(tp: Typ[_], peer: Targets.Value): (String,String)
}

trait AbstractDeviceTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSendSlave(tp: Typ[_]) : (String,String)
  def emitRecvSlave(tp: Typ[_]) : (String,String)
  //def emitSendViewSlave(tp: Typ[_]) : (String,String)
  //def emitRecvViewSlave(tp: Typ[_]) : (String,String)
  def emitSendUpdateSlave(tp: Typ[_]) : (String,String)
  def emitRecvUpdateSlave(tp: Typ[_]) : (String,String)

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
