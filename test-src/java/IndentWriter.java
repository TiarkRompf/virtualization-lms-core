package scala.virtualization.lms.epfl.test7;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;

class IndentWriter extends Writer {
    private Writer out;
    private int indent;
    private final char INDENT_INC = '{';
    private final char INDENT_DEC = '}';
    private final char INDENT_DO = '\n';
    private final String INDENT_STR = "  ";
    private boolean doIndent;

    public IndentWriter(Writer out) {
        this.out = out;
        this.indent = 0;
        this.doIndent = false;
    }

    public void write(char[] cbuf, int off, int len) throws IOException {
        // No knowledge about strings or anything else...
        for (int i=off; i<len; i++) {
            if (cbuf[i] == INDENT_DEC)
                indent--;

            if (doIndent)
                for (int j=0; j<indent; j++)
                    out.write(INDENT_STR);
            doIndent = false;

            out.write(cbuf[i]);

            if (cbuf[i] == INDENT_INC)
                indent++;
            else if (cbuf[i] == INDENT_DO)
                doIndent = true;
        }
    }

    public void close() throws IOException {
        out.close();
    }

    public void flush() throws IOException {
        out.flush();
    }

    public static PrintWriter getIndentPrintWriter(Writer out) {
        return new PrintWriter(new IndentWriter(out));
    }
}
