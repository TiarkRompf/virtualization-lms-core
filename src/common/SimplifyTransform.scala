package scala.virtualization.lms
package common

import scala.reflect.SourceContext


trait SimplifyTransform extends internal.FatScheduling {
  val IR: LoopsFatExp with IfThenElseFatExp
  import IR._
  
  // take a (sub) graph and a substitution, return a new graph with the substitution applied
  // (method transformAllFully)
  
  // TODO: check if this may fail if mirror creates more than one new stm (might be that internal stms are dropped)
  
  /*
     question: is the input part of globalDefs?
     question: should the input remain part of globalDefs?
     question: should the output be part of globalDefs?
  
     idea (if yes-yes-yes): use reifySubGraph to get newly created stms, union with old scope
      and use schedule to drop things no longer used
      
     other idea (if yes-no-yes): first remove input scope from globalDefs, 
      then if s = mirror(s) call reflectSubGraph(s) to insert stm into globalDefs
  */
  
  
/*
  def transformOneStmToGraph[A](stm: Stm, t: SubstTransformer): Exp[A] = stm match {
    case TP(s, d) =>
      val (res, stms) = reifySubGraph(transformOne(s,d,t))
  }
*/  

  def transformOne[A](s: Sym[A], x: Def[A], t: SubstTransformer): Exp[A] = {
    if (t.subst.contains(s)) return t(s) // should continue transforming t(s)?

    //if (!syms(x).exists(t.subst contains _)) return s   //<---- should be safe to prune but test fails (??)

    val y = try {
      val ss = syms(x)
      val tss = t(ss)
      if (ss != tss) {
        val s2 = mirror(x, t)(mtype(s.tp), mpos(s.pos))
        if (s2 == s) {
          printerr("warning: mirroring of "+s+"="+x+" syms " + ss.mkString(",") + " returned same object (expected t(syms) = " + tss.mkString(",") + ")")
        }
        s2 match { 
          case Def(x2) => 
            if (x.getClass == x2.getClass) {
              // if the def class does not change, we expect that the free syms are transformed
              val ss2 = syms(x2)
              if (ss2 != tss.filter(_.isInstanceOf[Sym[Any]])) // should do filtering in def of tss above?
                printerr("warning: mirroring of "+s+"="+x+" syms " + ss.mkString(",") + " returned "+s2+"="+x2+" syms " + ss2.mkString(",") + " (expected t(syms) = " + tss.mkString(",") + ")")
            }
            if (!(s2.tp <:< s.tp))
              printerr("warning: mirroring of "+s+"="+x+" type " + s.tp + " returned "+s2+"="+x2+" type " + s2.tp + " (not a subtype)")
          case _ =>
            if (!(s2.tp <:< s.tp))
              printerr("warning: mirroring of "+s+"="+x+" type " + s.tp + " returned "+s2+" type " + s2.tp + " (not a subtype)")
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
      case e: Throwable => 
        printerr("error: exception during mirroring of "+x+": "+ e)
        e.printStackTrace; 
        s
    }

    if (y != s) {
      if (y.isInstanceOf[Sym[Any]] && findDefinition(y.asInstanceOf[Sym[Any]]).nonEmpty)
        printdbg("--> replace " + s+"="+x + " by " + y+"="+findDefinition(y.asInstanceOf[Sym[Any]]).get.defines(y.asInstanceOf[Sym[Any]]).get)
      else
        printdbg("--> replace " + s+"="+x + " by " + y)
      t.subst(s) = y // TODO: move out of conditional?
    }
    y
  }

  def transformLoopBody[A](s: Sym[A], x: Def[A], t: SubstTransformer): Def[A] = {
    implicit val m: Manifest[A] = s.tp.asInstanceOf[Manifest[A]]
    mirrorFatDef(x, t)
  }
  def transformIfBody[A](s: Sym[A], x: Block[A], t: SubstTransformer): Block[A] = {
    implicit val m: Manifest[A] = s.tp.asInstanceOf[Manifest[A]]
    //transformOne(s,x,t)
    t(x)
  }
  
  // TODO: generalize, abstract out SimpleFatXX types
  def transformAll(scope: List[Stm], t: SubstTransformer): List[Stm] = {
    val scopeIndex = new java.util.IdentityHashMap[Sym[Any],Stm]
    for (stm <- scope; s <- stm.lhs) scopeIndex.put(s,stm)    

    scope flatMap {
      case TP(sym, rhs) =>
        transformOne(sym, rhs, t) match {
          case s: Sym[Any] => (Option(scopeIndex.get(s)) orElse findDefinition(s)).toList // check scope before target graph
          case _ => Nil
        }
      case TTP(lhs, mhs, SimpleFatIfThenElse(c,as,bs)) =>
        // alternate strategy: transform thin def, then fatten again (a little detour)
        printdbg("need to transform rhs of fat if/then/else: " + lhs + ", if " + c + " then " + as + " else " + bs)
        val lhs2 = (lhs zip mhs).map { case (s,r) => transformOne(s, r, t) }.distinct.asInstanceOf[List[Sym[Any]]]
        val mhs2 = lhs2.map(s => findDefinition(s).get.defines(s).get)
        // TBD: we're mirroring the defs in mhs, creating new stms
        // we don't really want new stms: if the defs are just abstract descriptions we only want them updated
        
        // this means we'll have both a TP and a TTP defining the same sym in globalDefs --> bad!
        // not quite so fast, chances are the TTP's never make it into globalDefs (no toAtom call)!!!
        
        if (lhs != lhs2) {
          val missing = Nil//(lhs2.map(s => findDefinition(s).get) diff innerScope)
          printdbg("lhs changed! will add to innerScope: "+missing.mkString(","))
          //innerScope = innerScope ::: missing
        }

        def infix_toIf(d: Def[Any]) = d match {
          case l: AbstractIfThenElse[_] => l
          case Reflect(l: AbstractIfThenElse[_], _, _) => l
        }
        val cond2 = if (lhs != lhs2) mhs2.map (_.toIf.cond) reduceLeft { (s1,s2) => assert(s1==s2,"conditions don't agree: "+s1+","+s2); s1 }
                    else t(c)
        val as2 = (if (lhs != lhs2) (lhs2 zip (mhs2 map (_.toIf.thenp)))
                   else (lhs zip as)) map { case (s,r) => transformIfBody(s,r,t) }
        val bs2 = (if (lhs != lhs2) (lhs2 zip (mhs2 map (_.toIf.elsep)))
                   else (lhs zip bs)) map { case (s,r) => transformIfBody(s,r,t) }
      
        printdbg("came up with: " + lhs2 + ", if " + cond2 + " then " + as2 + " else " + bs2 + " with subst " + t.subst.mkString(","))
        List(TTP(lhs2, mhs2, SimpleFatIfThenElse(cond2,as2,bs2)))
        
      case TTP(lhs, mhs, SimpleFatLoop(s,x,rhs)) =>
        // alternate strategy: transform thin def, then fatten again (a little detour)
        printdbg("need to transform rhs of fat loop: " + lhs + ", " + rhs)
        val lhs2 = (lhs zip mhs).map { case (s,r) => transformOne(s, r, t) }.distinct.asInstanceOf[List[Sym[Any]]]
        val mhs2 = lhs2.map(s => findDefinition(s).get.defines(s).get)
        if (lhs != lhs2) {
          val missing = (lhs2.map(s => findDefinition(s).get) diff scope/*innerScope*/)
          printdbg("lhs changed! will add to innerScope: "+missing.mkString(","))
          //innerScope = innerScope ::: missing
        }
        //val shape2 = if (lhs != lhs2) lhs2.map { case Def(SimpleLoop(s,_,_)) => s } reduceLeft { (s1,s2) => assert(s1==s2,"shapes don't agree: "+s1+","+s2); s1 }
        def infix_toLoop(d: Def[Any]) = d match {
          case l: AbstractLoop[_] => l
          case Reflect(l: AbstractLoop[_], _, _) => l
        }
        val shape2 = if (lhs != lhs2) mhs2.map (_.toLoop.size) reduceLeft { (s1,s2) => assert(s1==s2,"shapes don't agree: "+s1+","+s2); s1 }
                     else t(s)
        val rhs2 = (if (lhs != lhs2) (lhs2 zip (mhs2 map (_.toLoop.body)))
                    else (lhs zip rhs)) map { case (s,r) => transformLoopBody(s,r,t) }
        
  /*      //update innerScope -- change definition of lhs2 in place (necessary?)
        innerScope = innerScope map {
          case TP(l,_) if lhs2 contains l => TP(l, SimpleLoop(shape2,t(x).asInstanceOf[Sym[Int]],rhs2(lhs2.indexOf(l)))) 
          case d => d
        }*/
        
        printdbg("came up with: " + lhs2 + ", " + rhs2 + " with subst " + t.subst.mkString(","))
        List(TTP(lhs2, mhs2, SimpleFatLoop(shape2,t(x).asInstanceOf[Sym[Int]],rhs2)))
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
  }

  def transformAllFully(currentScope0: List[Stm], result0: List[Exp[Any]], t: SubstTransformer): (List[Stm], List[Exp[Any]]) = {
    var currentScope = currentScope0
    var result = result0
    
    // ---
    currentScope = getSchedule(currentScope)(currentScope) // clean things up!

    /*println("<1---"+result0+"/"+result)
    currentScope.foreach(println)
    println("---1>")*/

    // SIMPLIFY! <--- multiple steps necessary???
  
    def withEffectContext(body: =>List[Stm]): List[Stm] = {
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
    currentScope = getSchedule(currentScope)(currentScope) // clean things up!

    currentScope = withEffectContext { transformAll(currentScope, t) }
    result = t(result)
    currentScope = getSchedule(currentScope)(currentScope) // clean things up!

    currentScope = withEffectContext { transformAll(currentScope, t) }
    result = t(result)
    currentScope = getSchedule(currentScope)(result) // clean things up!


    // once more to see if we are converged
    val previousScope = currentScope
  
    currentScope = withEffectContext { transformAll(currentScope, t) }
    result = t(result)
    currentScope = getSchedule(currentScope)(result) // clean things up!
  
    if (currentScope != previousScope) { // check convergence
      printerr("error: transformation of scope contents has not converged")
      printdbg(previousScope + "-->" + currentScope)
    }
  
    /*println("<x---"+result0+"/"+result)
    currentScope.foreach(println)
    println("---x>")*/
    
    (currentScope, result)
  }
  
  
  def simplify(scope: List[Stm])(results: List[Exp[Any]]): (List[Stm], List[Exp[Any]]) = {
    val t = new SubstTransformer    
    val scope2 = transformAll(scope, t)
    val results2 = results map (t apply _)
    (scope2, results2)
  }
}
