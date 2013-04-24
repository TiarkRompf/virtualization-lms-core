package scala.lms
package util

import java.io._

trait ClosureCompare extends Externalizable {

  // the whole thing must be serializable, since embedded closures
  // might refer to the context.

  def writeExternal(out: ObjectOutput) {
//    println("in write object")
  }
  def readExternal(in: ObjectInput) {
    throw new NotSerializableException("this is just a mock-up!")
  }

  def canonicalize(f: Function[_,_]) = {
    val s = new java.io.ByteArrayOutputStream()
    val o = new java.io.ObjectOutputStream(s)
    o.writeObject(f)
    s.toString("ASCII")
  }

  def sameFunction(f: Function[_,_], g: Function[_,_]): Boolean = {

    def ser(f: Function[_,_]) = f.isInstanceOf[java.io.Serializable]

    if (f.getClass != g.getClass)
      return false

    if (ser(f) && ser(g)) {
      canonicalize(f) == canonicalize(g)
    } else {
      println("serizalizable(f): " + f.getClass + ": " + ser(f))
      println("serizalizable(g): " + g.getClass + ": " + ser(g))
      false
    }
  }


}