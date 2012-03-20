package scala.virtualization.lms
package common





// TODO: generalize and clean up

trait ForwardTransformer extends internal.FatBlockTraversal {
  val IR: LoopsFatExp with IfThenElseFatExp
  import IR._
  
  var subst: scala.collection.immutable.Map[Sym[_], Exp[_]] = Map.empty
  
  def transformBlock[A:Manifest](block: Block[A]): Block[A] = {
    reifyEffects {
      mirrorBlock(block)
    }
  }
  
  def mirrorBlock[A](block: Block[A]): Exp[A] = {
    traverseBlock(block)
    mirrorExp(block.res)
  }

  def mirrorExp[A](e: Exp[A]): Exp[A] = e match {
    case s: Sym[A] =>
      val e2 = subst.getOrElse(s,e).asInstanceOf[Exp[A]]
      if (e2 == e) e2 else mirrorExp(e2)
    case _ => e
  }

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) => 
      val sym2 = mirrorExp(sym)
      if (sym2 == sym) {
        val replace = mmmirror(rhs)
        subst = subst + (sym -> replace)
      }
  }
  

  // this one needs to reflect the contents of nested blocks, too
  // TODO: move somewhere else ?
  def mmmirror[A:Manifest](e: Def[A]): Exp[A] = e match {
    case IfThenElse(c,a,b) => __ifThenElse(mirrorExp(c),mirrorBlock(a),mirrorBlock(b))
    case _ => mirror(e, new Transformer { def apply[A](e: Exp[A]) = mirrorExp(e) })
  }
  
}


trait WorklistTransformer extends ForwardTransformer { // need backward version, too?
  val IR: LoopsFatExp with IfThenElseFatExp
  import IR._
  var curSubst: Map[Sym[Any],() => Exp[Any]] = Map.empty
  var nextSubst: Map[Sym[Any],() => Exp[Any]] = Map.empty
  def register[A](x: Exp[A])(y: => Exp[A]): Unit = {
    if (nextSubst.contains(x.asInstanceOf[Sym[A]]))
      println("already have a replacement for " + x)
    else {
      println("register replacement for " + x)
      nextSubst = nextSubst + (x.asInstanceOf[Sym[A]] -> (() => y))
    }
  }
  def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    if (nextSubst.isEmpty)
      println("nothing to do")
    subst = Map.empty
    curSubst = nextSubst
    nextSubst = Map.empty
    transformBlock(s)
  }
  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) => 
      curSubst.get(sym) match {
        case Some(replace) =>
          println("install replacement for " + sym)
          /*val b = reifyEffects(replace())
          println(b)
          mirrorBlock(b)*/
          subst = subst + (sym -> replace())
        case None => 
          super.traverseStm(stm)
      }
  }

}