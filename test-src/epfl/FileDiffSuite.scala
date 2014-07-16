package scala.virtualization.lms
package epfl

import java.io.{PrintStream,File,FileInputStream,FileOutputStream,ByteArrayOutputStream}
import org.scalatest._


trait FileDiffSuite extends Suite {
  val home = sys.env.get("LMS_HOME").map(_ + "/").getOrElse("")

  def withOutFile(name: String)(func: => Unit): Unit = {
    val file = new File(name)
    file.getParentFile.mkdirs()
    withOutput(new PrintStream(new FileOutputStream(file)))(func)
  }
  def captureOutput(func: => Unit): String = {
    val bstream = new ByteArrayOutputStream
    withOutput(new PrintStream(bstream))(func)
    bstream.toString
  }
  def withOutput(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      Console.withOut(out)(Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }
  
  def readFile(name: String): String = {
    val source = scala.io.Source.fromFile(name)
    val lines = source.getLines.mkString("\n")
    source.close()
    lines
  }
  def assertFileEqualsCheck(name: String): Unit = {
    assert(readFile(name) == readFile(name+".check"), name) // TODO: diff output
    new File(name) delete ()
  }
  def withOutFileChecked(name: String)(func: => Unit): Unit = {
    withOutFile(name)(func)
    assertFileEqualsCheck(name)
  }
}
