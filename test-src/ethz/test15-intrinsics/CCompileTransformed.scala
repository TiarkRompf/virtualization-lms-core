package ethz.test15

import java.io._

trait CCompileTransformed extends CCompile {

  def getCodeFileName () : (File, String) = {
    val codeFile = File.createTempFile("staged", ".c")
    val codeFileName = codeFile.getAbsolutePath()
    (codeFile, codeFileName)
  }

  def compile[B](f: List[Exp[Any]] => Exp[B]) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val (codeFile, _) = getCodeFileName()
    val stream = new PrintWriter(codeFile)
    codegen.emitTransformedSource(f, "staged", stream, false)
    stream.close()
    compileTransformedFile[B](codeFile)
  }

  def compileBlock[B] (b: (List[Sym[Any]], Block[B])) = {
    val codeFile = getCodeFileName()._1
    emitBlock(b, new PrintWriter(codeFile))
    compileTransformedFile[B](codeFile)(b._1.map(s => s.tp), b._2.tp)
  }

  def emitBlock[B] (block: (List[Sym[Any]], Block[B]), stream: PrintWriter, funcName: String = "staged") = {
    val b = block.asInstanceOf[(List[this.codegen.IR.Sym[Any]], this.codegen.Block[B])]
    codegen.emitTransformedBlock[B](b, funcName, stream)
  }

  def dumpCode[B] (name: String, block: (List[Sym[Any]], Block[B])) = emitBlock(block, new PrintWriter(name + ".c"))
}
