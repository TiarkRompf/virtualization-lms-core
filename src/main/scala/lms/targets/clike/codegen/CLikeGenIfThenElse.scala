package scala.lms
package targets.clike.codegen

import internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}
import ops.{BaseGenIfThenElse, BaseGenIfThenElseFat}

import java.io.PrintWriter

trait CudaGenIfThenElse extends CudaGenEffect with BaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case IfThenElse(c,a,b) =>
          // TODO: Not GPUable if the result is not primitive types.
          // TODO: Changing the reference of the output is dangerous in general.
          // TODO: In the future, consider passing the object references to the GPU kernels rather than copying by value.
          // Below is a safety check related to changing the output reference of the kernel.
          // This is going to be changed when above TODOs are done.
          //if( (sym==kernelSymbol) && (isObjectType(sym.tp)) ) throw new RuntimeException("CudaGen: Changing the reference of output is not allowed within GPU kernel.")

          val objRetType = (!isVoidType(sym.tp)) && (!isPrimitiveType(sym.tp))
          objRetType match {
            case true => throw new GenerationFailedException("CudaGen: If-Else cannot return object type.")
            case _ =>
          }
          isVoidType(sym.tp) match {
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
              stream.println("%s %s;".format(remap(sym.tp),quote(sym)))
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

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatIfThenElse(c,a,b) => sys.error("TODO: implement fat if CUDA codegen")
    case _ => super.emitFatNode(symList, rhs)
  }
}

trait OpenCLGenIfThenElse extends OpenCLGenEffect with BaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case IfThenElse(c,a,b) =>

          val objRetType = (!isVoidType(sym.tp)) && (!isPrimitiveType(sym.tp))
          objRetType match {
            case true => throw new GenerationFailedException("OpenCLGen: If-Else cannot return object type.")
            case _ =>
          }
          isVoidType(sym.tp) match {
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
              stream.println("%s %s;".format(remap(sym.tp),quote(sym)))
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

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatIfThenElse(c,a,b) => sys.error("TODO: implement fat if OpenCL codegen")
    case _ => super.emitFatNode(symList, rhs)
  }
}

trait CGenIfThenElse extends CGenEffect with BaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case IfThenElse(c,a,b) =>
        //TODO: using if-else does not work 
        remap(sym.tp) match {
          case "void" =>
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("} else {")
            emitBlock(b)
            stream.println("}")
          case _ =>
            stream.println("%s %s;".format(remap(sym.tp),quote(sym)))
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(a))))
            stream.println("} else {")
            emitBlock(b)
            stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(b))))
            stream.println("}")
        }
        /*
        val booll = remap(sym.tp).equals("void")
        if(booll) {
          stream.println("%s %s;".format(remap(sym.tp),quote(sym)))
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

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatIfThenElse(c,a,b) => sys.error("TODO: implement fat if C codegen")
    case _ => super.emitFatNode(symList, rhs)
  }
}

