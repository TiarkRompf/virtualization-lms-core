package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}
import scala.reflect.SourceContext

trait IfThenElse extends Base {
  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit ctx: SourceContext): Rep[T]

  // HACK -- bug in scala-virtualized
  override def __ifThenElse[T](cond: =>Boolean, thenp: => T, elsep: => T) = cond match {
    case true => thenp
    case false => elsep
  }
}

// TODO: it would be nice if IfThenElseExp would extend IfThenElsePureExp
// but then we would need to use different names.

trait IfThenElsePureExp extends IfThenElse with BaseExp {

  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]

  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit ctx: SourceContext) = IfThenElse(cond, thenp, elsep)
}


trait IfThenElseExp extends IfThenElse with EffectExp {

  abstract class AbstractIfThenElse[T] extends Def[T] {
    val cond: Exp[Boolean]
    val thenp: Exp[T]
    val elsep: Exp[T]
  }
  
  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends AbstractIfThenElse[T]

  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit ctx: SourceContext) = {
    val a = reifyEffectsHere(thenp)
    val b = reifyEffectsHere(elsep)
    ifThenElse(cond,a,b)
  }

  def ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: Rep[T], elsep: Rep[T])(implicit ctx: SourceContext) = { // thenp,elsep reified
    val ae = summarizeEffects(thenp)
    val be = summarizeEffects(elsep)
    reflectEffect(IfThenElse(cond,thenp,elsep), ae orElse be)
  }
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
    case Reflect(IfThenElse(c,a,b), u, es) => reflectMirrored(Reflect(IfThenElse(f(c),f(a),f(b)), mapOver(f,u), f(es)))
    case IfThenElse(c,a,b) => IfThenElse(f(c),f(a),f(b)) // FIXME: should apply pattern rewrites (ie call smart constructor)
    case _ => super.mirror(e,f)
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => syms(a):::syms(b)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }


  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case IfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    case _ => super.symsFreq(e)
  }

/*
  override def coldSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => syms(t) ++ syms(e)
    case _ => super.coldSyms(e)
  }
*/

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }

}

trait IfThenElseFatExp extends IfThenElseExp with BaseFatExp {

  abstract class AbstractFatIfThenElse extends FatDef {
    val cond: Exp[Boolean]
    val thenp: List[Exp[Any]]
    val elsep: List[Exp[Any]]
  }

  case class SimpleFatIfThenElse(cond: Exp[Boolean], thenp: List[Exp[Any]], elsep: List[Exp[Any]]) extends AbstractFatIfThenElse

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case SimpleFatIfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    case _ => super.symsFreq(e)
  }

  // aliasing / sharing
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => syms(a):::syms(b)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case SimpleFatIfThenElse(c,a,b) => Nil // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }
}


trait IfThenElseExpOpt extends IfThenElseExp { this: BooleanOpsExp with EqualExpBridge =>
  
  //TODO: eliminate conditional if both branches return same value!

  // it would be nice to handle rewrites in method ifThenElse but we'll need to
  // 'de-reify' blocks in case we rewrite if(true) to thenp. 
  // TODO: make reflect(Reify(..)) do the right thing
  
  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit ctx: SourceContext) = cond match {
    case Const(true) => thenp
    case Const(false) => elsep
    case Def(BooleanNegate(a)) => __ifThenElse(a, elsep, thenp)
    case Def(NotEqual(a,b)) => __ifThenElse(equals(a,b), elsep, thenp)
    case _ =>
      super.__ifThenElse(cond, thenp, elsep)
  }
}

trait BaseGenIfThenElse extends GenericNestedCodegen {
  val IR: IfThenElseExp
  import IR._

}

trait BaseGenIfThenElseFat extends BaseGenIfThenElse with GenericFatCodegen {
  val IR: IfThenElseFatExp
  import IR._

  override def fatten(e: TP[Any]): TTP = e.rhs match {
    case o: AbstractIfThenElse[_] => 
      TTP(List(e.sym), SimpleFatIfThenElse(o.cond, List(o.thenp), List(o.elsep)))
    case Reflect(o: AbstractIfThenElse[_], u, es) => //if !u.maySimple && !u.mayGlobal =>  // contrary, fusing will not change observable order
      // assume body will reflect, too...
      printdbg("-- fatten effectful if/then/else " + e)
      TTP(List(e.sym), SimpleFatIfThenElse(o.cond, List(o.thenp), List(o.elsep)))
    case _ => super.fatten(e)
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
}

trait ScalaGenIfThenElseFat extends ScalaGenIfThenElse with ScalaGenFat with BaseGenIfThenElseFat {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case SimpleFatIfThenElse(c,as,bs) => 
      def quoteList[T](xs: List[Exp[T]]) = if (xs.length > 1) xs.map(quote).mkString("(",",",")") else xs.map(quote).mkString(",")
      if (symList.length > 1) stream.println("// TODO: use vars instead of tuples to return multiple values")
      stream.println("val " + quoteList(symList) + " = if (" + quote(c) + ") {")
      emitFatBlock(as)
      stream.println(quoteList(as.map(getBlockResult)))
      stream.println("} else {")
      emitFatBlock(bs)
      stream.println(quoteList(bs.map(getBlockResult)))
      stream.println("}")
    case _ => super.emitFatNode(symList, rhs)
  }

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

trait CudaGenIfThenElseFat extends CudaGenIfThenElse with CudaGenFat with BaseGenIfThenElseFat {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case SimpleFatIfThenElse(c,a,b) => sys.error("TODO: implement fat if CUDA codegen")
    case _ => super.emitFatNode(symList, rhs)
  }
}

trait OpenCLGenIfThenElse extends OpenCLGenEffect with BaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case IfThenElse(c,a,b) =>

          val objRetType = (!isVoidType(sym.Type)) && (!isPrimitiveType(sym.Type))
          objRetType match {
            case true => throw new GenerationFailedException("OpenCLGen: If-Else cannot return object type.")
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

trait OpenCLGenIfThenElseFat extends OpenCLGenIfThenElse with OpenCLGenFat with BaseGenIfThenElseFat {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case SimpleFatIfThenElse(c,a,b) => sys.error("TODO: implement fat if OpenCL codegen")
    case _ => super.emitFatNode(symList, rhs)
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

trait CGenIfThenElseFat extends CGenIfThenElse with CGenFat with BaseGenIfThenElseFat {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    case SimpleFatIfThenElse(c,a,b) => sys.error("TODO: implement fat if C codegen")
    case _ => super.emitFatNode(symList, rhs)
  }
}

