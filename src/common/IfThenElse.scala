package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenEffect, GenericNestedCodegen, ScalaGenEffect}

trait IfThenElse extends Base {
  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]
}

// TODO: it would be nice if IfThenElseExp would extend IfThenElsePureExp
// but then we would need to give it a different name.

trait IfThenElsePureExp extends IfThenElse with BaseExp {

  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]

  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = IfThenElse(cond, thenp, elsep)
}


trait IfThenElseExp extends IfThenElse with EffectExp {

  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]

  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = {
    val a = reifyEffects(thenp)
    val b = reifyEffects(elsep)
    (a,b) match {
      case (Def(Reify(_,_)), _) | (_, Def(Reify(_,_))) => reflectEffect(IfThenElse(cond,a,b))
      case _ => IfThenElse(cond, thenp, elsep)
    }
  }

}

trait BaseGenIfThenElse extends GenericNestedCodegen {
  val IR: IfThenElseExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

 override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case IfThenElse(c, t, e) => getFreeVarBlock(c,Nil) ::: getFreeVarBlock(t,Nil) ::: getFreeVarBlock(e,Nil)
    case _ => super.getFreeVarNode(rhs)
  }
}

trait ScalaGenIfThenElse extends ScalaGenEffect with BaseGenIfThenElse {
  import IR._
 
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    /**
     * IfThenElse generates methods for each branch due to empirically discovered performance issues in the JVM
     * when generating long blocks of straight-line code in each branch.
     */
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = {")
      stream.println("def " + quote(sym) + "thenb() = {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("}")

      stream.println("def " + quote(sym) + "elseb() = {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

      stream.println("if (" + quote(c) + ") {")
      stream.println(quote(sym) + "thenb()")
      stream.println("} else {")
      stream.println(quote(sym) + "elseb()")
      stream.println("}")
      stream.println("}")
    
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenIfThenElse extends CudaGenEffect with BaseGenIfThenElse {
  //val IR: IfThenElseExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case IfThenElse(c,a,b) =>
          // TODO: Not GPUable if the result is not primitive types.
          // TODO: Changing the reference of the output is dangerous in general.
          // TODO: In the future, consider passing the object references to the GPU kernels rather than copying by value.
          // Below is a safety check related to changing the output reference of the kernel.
          // This is going to be changed when above TODOs are done.
          //if( (sym==kernelSymbol) && (isObjectType(sym.Type)) ) throw new RuntimeException("CudaGen: Changing the reference of output is not allowed within GPU kernel.")

          stream.println(addTab() + "if (" + quote(c) + ") {")
          tabWidth += 1
          addVarLink(getBlockResult(a).asInstanceOf[Sym[_]],sym)
          emitBlock(a)
          removeVarLink(getBlockResult(a).asInstanceOf[Sym[_]],sym)
          //stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(a))))
          tabWidth -= 1
          stream.println(addTab() + "} else {")
          tabWidth += 1
          addVarLink(getBlockResult(b).asInstanceOf[Sym[_]],sym)
          emitBlock(b)
          removeVarLink(getBlockResult(b).asInstanceOf[Sym[_]],sym)
          //stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(b))))
          tabWidth -= 1
          stream.println(addTab()+"}")
          allocReference(sym,getBlockResult(a).asInstanceOf[Sym[_]])
        
        case _ => super.emitNode(sym, rhs)
      }
    }
}