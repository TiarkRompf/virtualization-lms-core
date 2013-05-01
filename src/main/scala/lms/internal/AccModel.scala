package scala.lms
package internal

trait AbstractHostTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSend(tp:TypeRep[Any], host: Hosts.Value): (String,String)
  def emitRecv(tp:TypeRep[Any], host: Hosts.Value): (String,String)
  def emitSendView(tp:TypeRep[Any], host: Hosts.Value): (String,String)
  def emitRecvView(tp:TypeRep[Any], host: Hosts.Value): (String,String)
  def emitSendUpdate(tp:TypeRep[Any], host: Hosts.Value): (String,String)
  def emitRecvUpdate(tp:TypeRep[Any], host: Hosts.Value): (String,String)
}

trait AbstractDeviceTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSendSlave(tp:TypeRep[Any]) : (String,String)
  def emitRecvSlave(tp:TypeRep[Any]) : (String,String)
  //def emitSendViewSlave(tp:TypeRep[Any]) : (String,String)
  //def emitRecvViewSlave(tp:TypeRep[Any]) : (String,String)
  def emitSendUpdateSlave(tp:TypeRep[Any]) : (String,String)
  def emitRecvUpdateSlave(tp:TypeRep[Any]) : (String,String)

  //def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit
}

object Hosts extends Enumeration {

  val JVM = Value("jvm")
  val CPP = Value("cpp")

  def host(s: String): Value = s.toLowerCase() match {
    case "jvm" => JVM
    case "cpp" => CPP
    case _ => throw new IllegalArgumentException("unsupported host: " + s)
  }
}