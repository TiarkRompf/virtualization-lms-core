package scala.virtualization.lms.util

import java.io.IOException
import java.io.Writer

class IndentWriter(out: Writer) extends Writer {
    private var indent: Int = 0
    private val INDENT_INC: Char = '{'
    private val INDENT_DEC: Char = '}'
    private val INDENT_DO: Char = '\n'
    private val INDENT_STR: String = "  "
    private var doIndent: Boolean = false

    override def write(cbuf: Array[Char], off: Int, len:Int) {
      var i = off;
      var j = 0;

      while (i < off + len) {
        if (cbuf(i) == INDENT_DEC)
            indent -= 1;

        if (doIndent) {
          j = 0
          while(j < indent) {
            out.write(INDENT_STR)
            j += 1
          }
          doIndent = false
        }

        out.write(cbuf(i));

        if (cbuf(i) == INDENT_INC)
            indent += 1;
        else if (cbuf(i) == INDENT_DO)
            doIndent = true;

        i += 1
      }
    }

    def close() = out.close()
    def flush() = out.flush()
}
