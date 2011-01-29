package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenerationFailedException}

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
    val a = reifyEffectsHere(thenp)
    val b = reifyEffectsHere(elsep)
    (a,b) match {
      case (Def(Reify(_,_,_)), _) | (_, Def(Reify(_,_,_))) => reflectEffect(IfThenElse(cond,a,b)) //TODO u
      case _ => IfThenElse(cond, a, b)
    }
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
    case Reflect(IfThenElse(c,a,b), Global(), es) => reflectMirrored(Reflect(IfThenElse(f(c),f(a),f(b)), Global(), es map (e => f(e))))
    case IfThenElse(c,a,b) => IfThenElse(f(c),f(a),f(b))
    case _ => super.mirror(e,f)
  }

}

trait IfThenElseOpt extends IfThenElse with EffectExp { //TODO!
}


trait BaseGenIfThenElse extends GenericNestedCodegen {
  val IR: IfThenElseExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }
  
  override def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = rhs match {
    case IfThenElse(c, t, e) => getFreeVarBlock(c,Nil) ::: getFreeVarBlock(t,Nil) ::: getFreeVarBlock(e,Nil)
    case _ => super.getFreeVarNode(rhs)
  }
}

trait ScalaGenIfThenElse extends ScalaGenEffect with BaseGenIfThenElse {
  import IR._
 
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = if (" + quote(c) + ") {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("} else {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    
    case _ => super.emitNode(sym, rhs)
  }

/* TR: I think this should belong into delite

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    /**
     * IfThenElse generates methods for each branch due to empirically discovered performance issues in the JVM
     * when generating long blocks of straight-line code in each branch.
     */
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = {")
      stream.println("def " + quote(sym) + "thenb(): " + remap(getBlockResult(a).Type) + " = {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("}")

      stream.println("def " + quote(sym) + "elseb(): " + remap(getBlockResult(b).Type) + " = {")
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
*/
}

trait CudaGenIfThenElse extends CudaGenEffect with BaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case IfThenElse(c,a,b) =>
          // TODO: Not GPUable if the result is not primitive types.
          // TODO: Changing the reference of the output is dangerous in general.
          // TODO: In the future, consider passing the object references to the GPU kernels rather than copying by value.
          // Below is a safety check related to changing the output reference of the kernel.
          // This is going to be changed when above TODOs are done.
          //if( (sym==kernelSymbol) && (isObjectType(sym.Type)) ) throw new RuntimeException("CudaGen: Changing the reference of output is not allowed within GPU kernel.")

          val objRetType = (!isVoidType(sym.Type)) && (!isPrimitiveType(sym.Type))
          objRetType match {
            case true => throw new GenerationFailedException("CudaGen: If-Else cannot return object type.")
            case _ =>
          }
          isVoidType(sym.Type) match {
            case true =>
              stream.println(addTab() + "if (" + quote(c) + ") {")
              tabWidth += 1
              emitBlock(a)
              tabWidth -= 1
              stream.println(addTab() + "} else {")
              tabWidth += 1
              emitBlock(b)
              tabWidth -= 1
              stream.println(addTab()+"}")
            case false =>
              stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
              stream.println(addTab() + "if (" + quote(c) + ") {")
              tabWidth += 1
              emitBlock(a)
              stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(a))))
              tabWidth -= 1
              stream.println(addTab() + "} else {")
              tabWidth += 1
              emitBlock(b)
              stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(b))))
              tabWidth -= 1
              stream.println(addTab()+"}")
          }

        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CGenIfThenElse extends CGenEffect with BaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case IfThenElse(c,a,b) =>
        //TODO: using if-else does not work 
        remap(sym.Type) match {
          case "void" =>
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("} else {")
            emitBlock(b)
            stream.println("}")
          case _ =>
            stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(a))))
            stream.println("} else {")
            emitBlock(b)
            stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(b))))
            stream.println("}")
        }
        /*
        val booll = remap(sym.Type).equals("void")
        if(booll) {
          stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
          stream.println("if (" + quote(c) + ") {")
          emitBlock(a)
          stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(a))))
          stream.println("} else {")
          emitBlock(b)
          stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(b))))
          stream.println("}")
        }
        else {
          stream.println("if (" + quote(c) + ") {")
          emitBlock(a)
          stream.println("} else {")
          emitBlock(b)
          stream.println("}")
        }
        */
      case _ => super.emitNode(sym, rhs)
    }
  }
}