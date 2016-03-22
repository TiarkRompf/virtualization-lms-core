package scala.virtualization.lms
package util

import java.io.OutputStream

// The easiest kind of file writing!
class NullOutputStream() extends OutputStream {
  override def write(b: Int) { }
  override def write(b: Array[Byte]) { }
  override def write(b: Array[Byte], off: Int, len: Int) { }
}
