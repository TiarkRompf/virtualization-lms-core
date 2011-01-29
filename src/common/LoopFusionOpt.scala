package scala.virtualization.lms
package common

// TODO: simplify/transform trait should be generalized and cleaned up

trait SimplifyTransform extends internal.GenericFatCodegen {
  val IR: LoopsFatExp
  import IR._
  
  def transformOne[A](s: Sym[A], x: Def[A], t: SubstTransformer): Exp[A] = {
    if (t.subst.contains(s)) return t(s)
    implicit val m: Manifest[A] = s.Type.asInstanceOf[Manifest[A]]

    //if (!syms(x).exists(t.subst contains _)) return s   <---- should be safe to prune but test fails (??)

    val y = try { 
      val ss = syms(x)
      if (ss != t(ss))
        mirror(x, t) 
      else
        s
    } catch { case e => println("Exception during mirroring of "+x+": "+ e); e.printStackTrace; s }
    if (y != s) {

      if (y.isInstanceOf[Sym[Any]] && findDefinition(y.asInstanceOf[Sym[Any]]).nonEmpty)
        println("--> replace " + s+"="+x + " by " + y+"="+findDefinition(y.asInstanceOf[Sym[Any]]).get.rhs)
      else
        println("--> replace " + s+"="+x + " by " + y)

      t.subst(s) = y // TODO: move out of conditional?
    }
    y
  }

  def transformLoopBody[A](s: Sym[A], x: Def[A], t: SubstTransformer): Def[A] = {
    implicit val m: Manifest[A] = s.Type.asInstanceOf[Manifest[A]]
    mirrorFatDef(x, t)
  }
/*
  def transformOnePiece[A](s: Sym[A], x: Def[A], t: SubstTransformer): Def[A] = {
    mirror
  }
*/
  
  def transformAll(scope: List[TTP], t: SubstTransformer): List[TTP] = scope flatMap {
    case TTP(List(sym), ThinDef(rhs)) =>
      transformOne(sym, rhs, t) match {
        case s: Sym[Any] => List(fatten(findDefinition(s).get))
        case _ => Nil
      }
    case TTP(lhs, SimpleFatLoop(s,x,rhs)) =>
      // alternate strategy: transform thin def, then fatten again (a little detour)
      println("need to transform rhs of fat loop: " + lhs + ", " + rhs)
      val lhs2 = lhs.map { case s @ Def(r) => transformOne(s, r, t) }.distinct.asInstanceOf[List[Sym[Any]]]
      if (lhs != lhs2) println("lhs changed!")
      val rhs2 = lhs2.map { s => fatten(findDefinition(s).get) match { case TTP(List(s), SimpleFatLoop(_, _, List(r))) => transformLoopBody(s,r,t) }}
      println("came up with: " + lhs2 + ", " + rhs2 + " with subst " + t.subst.mkString(","))
      List(TTP(lhs2, SimpleFatLoop(t(s),t(x).asInstanceOf[Sym[Int]],rhs2)))
      // still problem: VectorSum(a,b) = SimpleLoop(i, ReduceElem(f(i))) 
      // might need to translate f(i), but looking up VectorSum will not be changed at all!!!
      // --> change rhs nonetheless???
      
      
/*      
      // potential problem: calling toAtom on a SimpleCollect (which does not have any symbol so far!)
      val lhs2 = (lhs zip rhs).map(p=>transformOne(p._1,p._2,t)).map { case s: Sym[Any] => s }.distinct.asInstanceOf[List[Sym[Any]]]
      val rhs2 = lhs2 map (findDefinition(_).get.rhs) //FIXME: will lookup old sym (ie VectorTrans) in case of AbstractCollect
      List(TTP(lhs2, SimpleFatLoop(t(s),t(x).asInstanceOf[Sym[Int]],rhs2)))
*/      
  }

  def simplify(scope: List[TTP])(results: List[Exp[Any]]): (List[TTP], List[Exp[Any]]) = {
    val t = new SubstTransformer    
    val scope2 = transformAll(scope, t)
    val results2 = results map (t apply _)
    (scope2, results2)
  }
}


trait LoopFusionOpt extends internal.GenericFatCodegen with SimplifyTransform {
  val IR: LoopsFatExp
  import IR._  
  
/*
  TODO: moved to GenericFatCodegen -- but they don't really belong there....
  
  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
*/

  object SimpleIndex {
    def unapply(a: Def[Any]): Option[(Exp[Any], Exp[Int])] = unapplySimpleIndex(a)
  }

  object SimpleCollect {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySimpleCollect(a)
  }

  override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: Exp[Any])(body: List[TTP] => A): A = {
    var result: Exp[Any] = result0
    var currentScope = currentScope0

    if (!shouldApplyFusion(currentScope)(result))
      return super.focusExactScopeFat(currentScope)(result)(body)
/*
    println("--- pre-pre-loop fusion: getFatSchedule")
    shallow = true
    val e2 = getFatSchedule(currentScope)(result)
    shallow = false
    e2.foreach(println)

    println("--- pre-pre-loop fusion: bound")
    val bound = currentScope.flatMap(z => boundSyms(z.rhs))
    bound.foreach(println)

    println("--- pre-pre-loop fusion: dependent on bound")
    val g1 = getFatDependentStuff(currentScope)(bound)
    g1.foreach(println)
*/



    // find loops at current top level
    var Wloops = super.focusExactScopeFat(currentScope)(result) { levelScope => 
      // TODO: cannot in general fuse several effect loops (one effectful and several pure ones is ok though)
      // so we need a strategy. a simple one would be exclude all effectful loops right away (TODO).
      levelScope collect { case e @ TTP(_, SimpleFatLoop(_,_,_)) => e }
    }
    
    // FIXME: more than one super call means exponential cost -- is there a better way?
    // ---> implicit memoization or explicit data structure
    
    /* problem: fusion might change currentScope quite drastically
       is there some kind of strength reduction transformation to go from here
       to the fused version without recomputing as much as we do now?
    */   
    
    var lcount = 0
    
    if (Wloops.nonEmpty) {
      var done = false

      // keep track of loops in inner scopes
      var UloopSyms = currentScope collect { case e @ TTP(lhs, SimpleFatLoop(_,_,_)) if !Wloops.contains(e) => lhs }
      
      do {
/*
        lcount += 1
        println("--- loop fusion: currentScope " + lcount)
        currentScope.foreach(println)

        println("--- loop fusion: Wloops " + lcount)
        Wloops.foreach(println)
*/
        // utils
        def WgetLoopVar(e: TTP): List[Sym[Int]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => List(x) }
        def WgetLoopRes(e: TTP): List[Def[Any]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => rhs }

        val loopSyms = Wloops flatMap (_.lhs)
        val loopVars = Wloops flatMap WgetLoopVar

        val WloopSyms = Wloops map (_.lhs)
        val WloopVars = Wloops map WgetLoopVar

        
        // find negative dependencies (those that block fusion)
        
        // might be costly: resolve and traverse complete input deps for each loop body
        // O(nloops*currentScope.length)   assuming schedule is linear (NOT true currently)
        
        val WtableNeg = Wloops.flatMap { dx => // find non-simple dependencies
          val thisLoopSyms = WgetLoopVar(dx)
          val otherLoopSyms = loopSyms diff (dx.lhs)
          getFatSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
            case e@TTP(_, ThinDef(SimpleIndex(a,i))) if (thisLoopSyms contains i) => 
              //println("ignoring simple dependency " + e + " on loop var " + thisLoopSyms)
              Nil // direct deps on this loop's induction var don't count
            case sc =>
              val pr = syms(sc.rhs).intersect(otherLoopSyms) flatMap { otherLoop => dx.lhs map ((otherLoop, _)) }
              if (pr.nonEmpty) println("fusion of "+pr+" prevented by " + sc + " which is required by body of " + dx.lhs)
              pr
          }
        }.distinct
      
        // TODO: have WtableNeg keep track of the statements that prevent fusion
        // then we can later remove the entry and see if the dependency goes away...
      
        println("wtableneg: " + WtableNeg)        
        
        // partitioning: build maximal sets of loops to be fused
        
        def canFuse(a: TTP, b: TTP): Boolean = {
          if (!canFuseRhs(a.rhs, b.rhs)) return false
          val cartesian = a.lhs.flatMap(x=>b.lhs.flatMap(y=>List((x,y),(y,x))))
          (WtableNeg intersect cartesian).isEmpty
        }
        
        // TODO: improve  !WtableNeg.exists(p => (a.lhs contains p._1) && (b.lhs contains.p_2)
        //                                       (b.lhs contains p._1) && (a.lhs contains.p_2))
      
        def canFuseRhs(a: FatDef, b: FatDef): Boolean = (a,b) match { // only fuse loops of same size
          case (SimpleFatLoop(s1,_,_), SimpleFatLoop(s2,_,_)) => s1 == s2
          case _ => false
        }
        
        val t = new SubstTransformer

        var partitionsIn = Wloops
        var partitionsOut = Nil:List[TTP]
      
        for (b <- partitionsIn) {
          // try to add to an item in partitionsOut, if not possible add as-is
          partitionsOut.find(a => canFuse(a,b)) match {
            case Some(a) => 
              val shape = a.rhs match { case SimpleFatLoop(s,_,_) => s } // safe assumption??
              val targetVar = WgetLoopVar(a)(0)
              //currentScope = currentScope ::: (WgetLoopVar(b) map (v => TTP(List(v), ThinDef(Copy(targetVar))))) // TODO: maybe not here?
              for (w <- WgetLoopVar(b))
                t.subst(w) = targetVar
              
              val fused = TTP(a.lhs:::b.lhs, SimpleFatLoop(shape, targetVar, WgetLoopRes(a):::WgetLoopRes(b)))
              partitionsOut = fused :: (partitionsOut diff List(a))
            case None => partitionsOut = b::partitionsOut
          }
        }
      
        println("partitions: " + partitionsOut)
      
      
        // actually do the fusion: 
      
        if ((partitionsOut intersect partitionsIn) != partitionsOut) { // was there any change?

          // equalize loop variables (TODO! <-- wait, isn't that done???)

          // within fused loops, remove accesses to outcomes of the fusion

          currentScope.foreach {
            case e@TTP(List(s), ThinDef(SimpleIndex(a, i))) =>
              println("considering " + e)
              partitionsOut.find(_.lhs contains a) match {
                case Some(fused) if WgetLoopVar(fused) contains t(i) => 
                  val index = fused.lhs.indexOf(a)
                  
                  println("replace " + e + " at " + index + " within " + fused)

                  val rhs = WgetLoopRes(fused)(index) match { case SimpleCollect(y) => y }
                  
                  t.subst(s) = rhs
                case _ => //e
              }
            case _ => //e
          }
          
          currentScope = getFatSchedule(currentScope)(result) // clean things up!

          // SIMPLIFY! <--- multiple steps necessary???
          
          currentScope = transformAll(currentScope, t)
          result = t(result)
          currentScope = getFatSchedule(currentScope)(result) // clean things up!

          currentScope = transformAll(currentScope, t)
          result = t(result)
          currentScope = getFatSchedule(currentScope)(result) // clean things up!
          
          //Wloops = currentScope collect { case e @ TTP(_, FatLoop(_,_,_)) => e }

          Wloops = transformAll(partitionsOut, t)
          
          UloopSyms = UloopSyms map (_ map (e => t(e)) collect { case s: Sym[Any] => s } ) // just lookup the symbols
          
          println("try once more ...")
        } else {
          println("no changes, we're done")
          done = true
        }
      
      } while (!done)
     
      
      // prune Wloops (some might be no longer necessary!)
      Wloops = Wloops map {
        case TTP(lhs, SimpleFatLoop(s, x, rhs)) =>
          val ex = lhs map (s => currentScope exists (_.lhs == List(s)))
          def select[A](a: List[A], b: List[Boolean]) = (a zip b) collect { case (w, true) => w }
          TTP(select(lhs, ex), SimpleFatLoop(s, x, select(rhs, ex)))
      }
      
      // FIXME: don't throw out all loops, might have some that are *not* in levelScope
      // note: if we don't do it here, we will likely see a problem going back to innerScope in 
      // FatCodegen.focusExactScopeFat below. --> how to go back from SimpleFatLoop to VectorPlus??
      // UPDATE: UloopSyms puts a tentative fix in place. check if it is sufficient!!
      // what is the reason we cannot just look at Wloops??
      currentScope = currentScope.filter { case e@TTP(lhs, _: AbstractFatLoop) => 
        val keep = UloopSyms contains lhs
        //if (!keep) println("dropping: " + e + ", not int UloopSyms: " + UloopSyms)
        keep case _ => true } ::: Wloops

      // schedule (and emit)
      currentScope = getFatSchedule(currentScope)(result) // clean things up!
     
    }

    // do what super does ...
    super.focusExactScopeFat(currentScope)(result)(body)
  }

}
