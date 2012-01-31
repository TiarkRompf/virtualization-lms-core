package scala.virtualization.lms
package common

// TODO: generalize and clean up

trait SimplifyTransform extends internal.GenericFatCodegen {
  val IR: LoopsFatExp with IfThenElseFatExp
  import IR._
  
  case class Forward[A](x: Exp[A]) extends Def[A]
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: java.io.PrintWriter) = rhs match {
    case Forward(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
  
  def transformOne[A](s: Sym[A], x: Def[A], t: SubstTransformer): Exp[A] = {
    if (t.subst.contains(s)) return t(s)
    implicit val m: Manifest[A] = s.Type.asInstanceOf[Manifest[A]]

    //if (!syms(x).exists(t.subst contains _)) return s   //<---- should be safe to prune but test fails (??)

    val y = try { 
      val ss = syms(x)
      val tss = t(ss)
      if (ss != tss) {
        val s2 = mirror(x, t) 
        if (s2 == s) {
          printerr("warning: mirroring of "+s+"="+x+" syms " + ss.mkString(",") + " returned same object (expected t(syms) = " + tss.mkString(",") + ")")
        }
        s2 match { 
          case Def(x2) => 
            val ss2 = syms(x2)
            if (ss2 != tss.filter(_.isInstanceOf[Sym[Any]])) // should do filtering in def of tss above?
              printerr("warning: mirroring of "+s+"="+x+" syms " + ss.mkString(",") + " returned "+s2+"="+x2+" syms " + ss2.mkString(",") + " (expected t(syms) = " + tss.mkString(",") + ")")
            if (!(s2.Type <:< s.Type))
              printerr("warning: mirroring of "+s+"="+x+" type " + s.Type + " returned "+s2+"="+x2+" type " + s2.Type + " (not a subtype)")
          case _ =>
            if (!(s2.Type <:< s.Type))
              printerr("warning: mirroring of "+s+"="+x+" type " + s.Type + " returned "+s2+" type " + s2.Type + " (not a subtype)")
        }
        s2
      } else {
        printdbg("skipping mirror operation "+s+"="+x+" syms " + ss.mkString(",") + " subst " + t.subst.mkString(","))
        s
      }
    } catch { //hack
      case e if e.toString contains "don't know how to mirror" => 
        printerr("error: " + e.getMessage)
        s
      case e => 
        printerr("error: exception during mirroring of "+x+": "+ e)
        e.printStackTrace; 
        s
    }

    if (y != s) {
      if (y.isInstanceOf[Sym[Any]] && findDefinition(y.asInstanceOf[Sym[Any]]).nonEmpty)
        printdbg("--> replace " + s+"="+x + " by " + y+"="+findDefinition(y.asInstanceOf[Sym[Any]]).get.rhs)
      else
        printdbg("--> replace " + s+"="+x + " by " + y)
      t.subst(s) = y // TODO: move out of conditional?
    }
    y
  }

  def transformLoopBody[A](s: Sym[A], x: Def[A], t: SubstTransformer): Def[A] = {
    implicit val m: Manifest[A] = s.Type.asInstanceOf[Manifest[A]]
    mirrorFatDef(x, t)
  }
  def transformIfBody[A](s: Sym[A], x: Exp[A], t: SubstTransformer): Exp[A] = {
    implicit val m: Manifest[A] = s.Type.asInstanceOf[Manifest[A]]
    //transformOne(s,x,t)
    t(x)
  }
  
  // TODO: generalize, abstract out SimpleFatXX types
  def transformAll(scope: List[TTP], t: SubstTransformer): List[TTP] = scope flatMap {
    case TTP(List(sym), ThinDef(rhs)) =>
      transformOne(sym, rhs, t) match {
        case s: Sym[Any] => 
          val tp = findDefinition(s)
          if (tp.isEmpty) Nil // happens for array(100) { i => i } TODO: match on Def?
          else List(fatten(tp.get))
        case _ => Nil
      }
    case TTP(lhs, SimpleFatIfThenElse(c,as,bs)) =>
      // alternate strategy: transform thin def, then fatten again (a little detour)
      printdbg("need to transform rhs of fat if/then/else: " + lhs + ", if " + c + " then " + as + " else " + bs)
      val lhs2 = lhs.map { case s @ Def(r) => transformOne(s, r, t) }.distinct.asInstanceOf[List[Sym[Any]]]
      if (lhs != lhs2) {
        val missing = (lhs2.map(s => findDefinition(s).get) diff innerScope)
        printdbg("lhs changed! will add to innerScope: "+missing.mkString(","))
        innerScope = innerScope ::: missing
      }
      //val shape2 = if (lhs != lhs2) lhs2.map { case Def(SimpleLoop(s,_,_)) => s } reduceLeft { (s1,s2) => assert(s1==s2,"shapes don't agree: "+s1+","+s2); s1 }
      //val cond2 = if (lhs != lhs2) lhs2.map { s => fatten(findDefinition(s).get) match { case TTP(List(s), SimpleFatIfThenElse(c, _, _)) => c }} reduceLeft { (s1,s2) => assert(s1==s2,"conditions don't agree: "+s1+","+s2); s1 }
      val cond2 = if (lhs != lhs2) lhs2.map { 
                    case Def(l: AbstractIfThenElse[_]) => l.cond
                    case Def(Reflect(l: AbstractIfThenElse[_], _, _)) => l.cond
                    case Def(x) => sys.error("XXX " + x)
                  } reduceLeft { (s1,s2) => assert(s1==s2,"conditions don't agree: "+s1+","+s2); s1 }
                  else t(c)
      val as2 = if (lhs != lhs2) lhs2.map { s => fatten(findDefinition(s).get) match { case TTP(List(s), SimpleFatIfThenElse(_, List(r), _)) => transformIfBody(s,r,t) }}
                 else (lhs zip as) map { case (s,r) => transformIfBody(s,r,t) }
      val bs2 = if (lhs != lhs2) lhs2.map { s => fatten(findDefinition(s).get) match { case TTP(List(s), SimpleFatIfThenElse(_, _, List(r))) => transformIfBody(s,r,t) }}
                 else (lhs zip bs) map { case (s,r) => transformIfBody(s,r,t) }
    
      //update innerScope -- change definition of lhs2 in place
      innerScope = innerScope map {
        case TP(l,_) if lhs2 contains l => TP(l, IfThenElse(cond2,as2(lhs2.indexOf(l)),bs2(lhs2.indexOf(l))))
        case d => d
      }
    
      printdbg("came up with: " + lhs2 + ", if " + cond2 + " then " + as2 + " else " + bs2 + " with subst " + t.subst.mkString(","))
      List(TTP(lhs2, SimpleFatIfThenElse(cond2,as2,bs2)))
    case TTP(lhs, SimpleFatLoop(s,x,rhs)) =>
      // alternate strategy: transform thin def, then fatten again (a little detour)
      printdbg("need to transform rhs of fat loop: " + lhs + ", " + rhs)
      val lhs2 = lhs.map { case s @ Def(r) => transformOne(s, r, t) }.distinct.asInstanceOf[List[Sym[Any]]]
      if (lhs != lhs2) {
        val missing = (lhs2.map(s => findDefinition(s).get) diff innerScope)
        printdbg("lhs changed! will add to innerScope: "+missing.mkString(","))
        innerScope = innerScope ::: missing
      }
      //val shape2 = if (lhs != lhs2) lhs2.map { case Def(SimpleLoop(s,_,_)) => s } reduceLeft { (s1,s2) => assert(s1==s2,"shapes don't agree: "+s1+","+s2); s1 }
      val shape2 = if (lhs != lhs2) lhs2.map { case Def(l: AbstractLoop[_]) => l.size case Def(Reflect(l: AbstractLoop[_], _, _)) => l.size } reduceLeft { (s1,s2) => assert(s1==s2,"shapes don't agree: "+s1+","+s2); s1 }
                   else t(s)
      val rhs2 = if (lhs != lhs2) lhs2.map { s => fatten(findDefinition(s).get) match { case TTP(List(s), SimpleFatLoop(_, _, List(r))) => transformLoopBody(s,r,t) }}
                 else (lhs zip rhs) map { case (s,r) => transformLoopBody(s,r,t) }
      
      //update innerScope -- change definition of lhs2 in place
      innerScope = innerScope map {
        case TP(l,_) if lhs2 contains l => TP(l, SimpleLoop(shape2,t(x).asInstanceOf[Sym[Int]],rhs2(lhs2.indexOf(l)))) 
        case d => d
      }
      
      printdbg("came up with: " + lhs2 + ", " + rhs2 + " with subst " + t.subst.mkString(","))
      List(TTP(lhs2, SimpleFatLoop(shape2,t(x).asInstanceOf[Sym[Int]],rhs2)))
      // still problem: VectorSum(a,b) = SimpleLoop(i, ReduceElem(f(i))) 
      // might need to translate f(i), but looking up VectorSum will not be changed at all!!!
      // --> change rhs nonetheless???
      
/*      
      // potential problem here: calling toAtom on a SimpleCollect (which does not have any symbol so far!)
      val lhs2 = (lhs zip rhs).map(p=>transformOne(p._1,p._2,t)).map { case s: Sym[Any] => s }.distinct.asInstanceOf[List[Sym[Any]]]
      val rhs2 = lhs2 map (findDefinition(_).get.rhs) //FIXME: will lookup old sym (ie VectorTrans) in case of AbstractCollect
      List(TTP(lhs2, SimpleFatLoop(t(s),t(x).asInstanceOf[Sym[Int]],rhs2)))
*/      
  }

  def transformAllFully(currentScope0: List[TTP], result0: List[Exp[Any]], t: SubstTransformer): (List[TTP], List[Exp[Any]]) = {
    var currentScope = currentScope0
    var result = result0
    
    // ---
    currentScope = getFatSchedule(currentScope)(currentScope) // clean things up!

    /*println("<1---"+result0+"/"+result)
    currentScope.foreach(println)
    println("---1>")*/

    // SIMPLIFY! <--- multiple steps necessary???
  
    def withEffectContext(body: =>List[TTP]): List[TTP] = {
      val save = context
      context = Nil
      val scope = body
      val leftovereffects = context.filterNot((scope.flatMap(_.lhs)) contains _)
      if (leftovereffects.nonEmpty) 
        printlog("warning: transformation left effect context (will be discarded): "+leftovereffects)
      context = save
      scope
    }
  
    currentScope = withEffectContext { transformAll(currentScope, t) }
    result = t(result)
    currentScope = getFatSchedule(currentScope)(currentScope) // clean things up!

    currentScope = withEffectContext { transformAll(currentScope, t) }
    result = t(result)
    currentScope = getFatSchedule(currentScope)(currentScope) // clean things up!

    currentScope = withEffectContext { transformAll(currentScope, t) }
    result = t(result)
    currentScope = getFatSchedule(currentScope)(result) // clean things up!


    // once more to see if we are converged
    val previousScope = currentScope
  
    currentScope = withEffectContext { transformAll(currentScope, t) }
    result = t(result)
    currentScope = getFatSchedule(currentScope)(result) // clean things up!
  
    if (currentScope != previousScope) { // check convergence
      printerr("error: transformation of scope contents has not converged")
      printdbg(previousScope + "-->" + currentScope)
    }
  
    /*println("<x---"+result0+"/"+result)
    currentScope.foreach(println)
    println("---x>")*/
    
    (currentScope, result)
  }
  
  
  def simplify(scope: List[TTP])(results: List[Exp[Any]]): (List[TTP], List[Exp[Any]]) = {
    val t = new SubstTransformer    
    val scope2 = transformAll(scope, t)
    val results2 = results map (t apply _)
    (scope2, results2)
  }
}