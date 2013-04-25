package scala.lms
package internal

trait AbstractHostTransfer {
  this: GenericCodegen =>

  val IR: Expressions
  import IR._

  def emitSend(tp: Manifest[Any], host: Hosts.Value): (String,String)
  def emitRecv(tp: Manifest[Any], host: Hosts.Value): (String,String)
  def emitSendView(tp: Manifest[Any], host: Hosts.Value): (String,String)
  def emitRecvView(tp: Manifest[Any], host: Hosts.Value): (String,String)
  def emitSendUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String)
  def emitRecvUpdate(tp: Manifest[Any], host: Hosts.Value): (String,String)
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

object Hosts extends Enumeration {

  val JVM = Value("jvm")
  val CPP = Value("cpp")

  def host(s: String): Value = s.toLowerCase() match {
    case "jvm" => JVM
    case "cpp" => CPP
    case _ => throw new IllegalArgumentException("unsupported host: " + s)
  }
}