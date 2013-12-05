package ethz.test15

//import ch.ethz.spirals.rewrites.CodeTransformer
import java.io._

trait CCompileTransformed extends CCompile {

  class CodeTransformer

  protected def getCodeFileName () : (File, String) = {
    val codeFile = File.createTempFile("staged", ".c")
    val codeFileName = codeFile.getAbsolutePath()
    (codeFile, codeFileName)
  }

  def compile[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A], mB: Manifest[B]) = {
    val func: (List[Exp[Any]] => Exp[B]) = (in: List[Exp[Any]]) => f(in(0).asInstanceOf[Exp[A]])
    implicit val mList = List(mA).asInstanceOf[List[Manifest[Any]]]
    compile[B](func)
  }

  def compile[B](f: List[Exp[Any]] => Exp[B])(implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    compileTransformed(f, List.empty[CodeTransformer])
  }

  def emitTransformedBlock[B] (block: (List[Sym[Any]], Block[B]), stream: PrintWriter)(implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val (codeFile, codeFileName) = getCodeFileName()
    if ( debug ) System.err.println("Dumping the code to: " + codeFileName);
    val b = block.asInstanceOf[(List[this.codegen.IR.Sym[Any]], this.codegen.Block[B])]
    codegen.emitTransformedBlock[B](b, "staged", stream)
    stream.close()
  }

  def dumpCode[B] (name: String, block: (List[Sym[Any]], Block[B]))(implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val stream = new PrintWriter(name + ".c")
    if ( debug ) System.err.println("Dumping the code to: " + name);
    val b = block.asInstanceOf[(List[this.codegen.IR.Sym[Any]], this.codegen.Block[B])]
    codegen.emitTransformedBlock[B](b, name, stream)
    stream.close()
  }

  def compileTransformedBlock[A, B] (block: (Sym[A], Block[B]))(implicit mA: Manifest[A], mB: Manifest[B]) = {
    val symList = List(block._1).asInstanceOf[List[Sym[Any]]]
    implicit val mList = List(mA).asInstanceOf[List[Manifest[Any]]]
    compileTransformedBlock[B]((symList, block._2))
  }

  def compileTransformedBlock[B] (block: (List[Sym[Any]], Block[B]))(implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val (codeFile, codeFileName) = getCodeFileName()
    val stream = new PrintWriter(codeFile)
    if ( debug ) System.err.println("Dumping the code to: " + codeFileName);
    val b = block.asInstanceOf[(List[this.codegen.IR.Sym[Any]], this.codegen.Block[B])]
    codegen.emitTransformedBlock[B](b, "staged", stream, false)
    stream.close()
    compileTransformedFile[B](codeFile)
  }

  def compileTransformed[B](f: List[Exp[Any]] => Exp[B], transformers: List[CodeTransformer])(implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val (codeFile, codeFileName) = getCodeFileName()
    val stream = new PrintWriter(codeFile)
    if ( debug ) System.err.println("Dumping the code to: " + codeFileName);
    codegen.emitTransformedSource(f, "staged", stream, /*transformers,*/ false)
    stream.close()
    compileTransformedFile[B](codeFile)
  }
}
