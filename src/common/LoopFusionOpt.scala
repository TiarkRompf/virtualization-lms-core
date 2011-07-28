package scala.virtualization.lms
package common

// TODO: simplify/transform trait should be generalized and cleaned up

trait SimplifyTransform extends internal.GenericFatCodegen {
  val IR: LoopsFatExp
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
        if (s2 == s)
          printerr("warning: mirroring of "+s+"="+x+" syms " + ss.mkString(",") + " returned same object (expected t(syms) = " + tss.mkString(",") + ")")
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
  
  def transformAll(scope: List[TTP], t: SubstTransformer): List[TTP] = scope flatMap {
    case TTP(List(sym), ThinDef(rhs)) =>
      transformOne(sym, rhs, t) match {
        case s: Sym[Any] => 
          val tp = findDefinition(s)
          if (tp.isEmpty) Nil // happens for array(100) { i => i } TODO: match on Def?
          else List(fatten(tp.get))
        case _ => Nil
      }
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
      val shape2 = if (lhs != lhs2) lhs2.map { case Def(l: AbstractLoop[_]) => l.size } reduceLeft { (s1,s2) => assert(s1==s2,"shapes don't agree: "+s1+","+s2); s1 }
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
  def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = None
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
  def unapplySimpleCollectIf(e: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = None
*/

  object SimpleIndex {
    def unapply(a: Def[Any]): Option[(Exp[Any], Exp[Int])] = unapplySimpleIndex(a)
  }

  object SimpleDomain {
    def unapply(a: Def[Int]): Option[Exp[Any]] = unapplySimpleDomain(a)
  }

  object SimpleCollect {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySimpleCollect(a)
  }

  object SimpleCollectIf {
    def unapply(a: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollectIf(a)
  }


  override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: List[Exp[Any]])(body: List[TTP] => A): A = {
    var result: List[Exp[Any]] = result0
    var currentScope = currentScope0

    if (!shouldApplyFusion(currentScope)(result))
      return super.focusExactScopeFat(currentScope)(result)(body)
/*
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
    
    if (Wloops.nonEmpty) {
      var done = false

      // keep track of loops in inner scopes
      var UloopSyms = currentScope collect { case e @ TTP(lhs, SimpleFatLoop(_,_,_)) if !Wloops.contains(e) => lhs }
      
      do {
        // utils
        def WgetLoopShape(e: TTP): Exp[Int] = e.rhs match { case SimpleFatLoop(s,x,rhs) => s }
        def WgetLoopVar(e: TTP): List[Sym[Int]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => List(x) }
        def WgetLoopRes(e: TTP): List[Def[Any]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => rhs }

        val loopSyms = Wloops flatMap (_.lhs)
        val loopVars = Wloops flatMap WgetLoopVar

        val WloopSyms = Wloops map (_.lhs)
        val WloopVars = Wloops map WgetLoopVar

        // find negative dependencies (those that block fusion)
        
        // might be costly: resolve and traverse complete input deps for each loop body
        // O(nloops*currentScope.length), assuming schedule is linear (NOT true currently!)

        // possible extension: have WtableNeg keep track of the statements that prevent fusion
        // then we can later remove the entry and see if the dependency goes away...
        
        val WtableNeg = Wloops.flatMap { dx => // find non-simple dependencies (other than a(i))
          val thisLoopSyms = WgetLoopVar(dx)
          val otherLoopSyms = loopSyms diff (dx.lhs)
          getFatSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
            case e@TTP(_, ThinDef(SimpleIndex(a,i))) if (thisLoopSyms contains i) => 
              //println("ignoring simple dependency " + e + " on loop var " + thisLoopSyms)
              Nil // direct deps on this loop's induction var don't count
            case sc =>
              val pr = syms(sc.rhs).intersect(otherLoopSyms) flatMap { otherLoop => dx.lhs map ((otherLoop, _)) }
              if (pr.nonEmpty) printlog("fusion of "+pr+" prevented by " + sc + " which is required by body of " + dx.lhs)
              pr
          }
        }.distinct      
      
        printlog("wtableneg: " + WtableNeg)        
        
        // other preconditions for fusion: loops must have same shape, or one must loop over the other's result
        
        def canFuseIndirect(a: TTP, b: TTP): Boolean = 
          !WtableNeg.exists(p => (a.lhs contains p._1) && (b.lhs contains p._2) || (b.lhs contains p._1) && (a.lhs contains p._2))
        
        def canFuseDirect(a: TTP, b: TTP): Boolean = (a.rhs,b.rhs) match {
          case (SimpleFatLoop(s1,_,_), SimpleFatLoop(s2,_,_)) if s1 == s2 => true  // same size (horizontal or pipeline)
          case (SimpleFatLoop(Def(SimpleDomain(a1)),_,_), SimpleFatLoop(_,_,_)) if b.lhs contains a1 => true // pipeline
          case (SimpleFatLoop(_,_,_), SimpleFatLoop(Def(SimpleDomain(b1)),_,_)) if a.lhs contains b1 => true
          case _ => false
        }
        
        def canFuse(a: TTP, b: TTP): Boolean = canFuseDirect(a,b) && canFuseIndirect(a,b)        
        
        // shape dependency helpers
        
        def isShapeDep(s: Exp[Int], a: TTP) = s match { case Def(SimpleDomain(a1)) => a.lhs contains a1 case _ => false }
        def getShapeCond(s: Exp[Int], a: TTP) = s match { case Def(SimpleDomain(a1)) => WgetLoopRes(a)(a.lhs indexOf a1) match { case SimpleCollectIf(a,c) => c } }
        
        def extendLoopWithCondition(e: TTP, shape: Exp[Int], targetVar: Sym[Int], c: List[Exp[Boolean]]): List[Exp[Any]] = e.rhs match { 
          case SimpleFatLoop(s,x,rhs) => rhs.map { r => findOrCreateDefinition(SimpleLoop(shape,targetVar,applyAddCondition(r,c))).sym }
        }
        
        // partitioning: build maximal sets of loops to be fused
        // already fuse loops headers (shape, index variables)
        
        val t = new SubstTransformer

        var partitionsIn = Wloops
        var partitionsOut = Nil:List[TTP]
      
        for (b <- partitionsIn) {
          // try to add to an item in partitionsOut, if not possible add as-is
          partitionsOut.find(a => canFuse(a,b)) match {
            case Some(a) => 
              val shapeA = WgetLoopShape(a)
              val shapeB = WgetLoopShape(b)
              
              // unify loop vars
              val targetVar = WgetLoopVar(a)(0) // should use a fresh var?
              for (w <- WgetLoopVar(b))
                t.subst(w) = targetVar
              
              // analyze shape dependency and add appropriate conditions to loop body when fusing a filter loop
              val shape = if (isShapeDep(shapeA,b)) {
                val loops2 = extendLoopWithCondition(a,shapeB,targetVar,getShapeCond(shapeA,b))
                (a.lhs zip loops2) foreach { p => t.subst(p._1) = p._2 }
                shapeB
              } else if (isShapeDep(shapeB,a)) {
                val loops2 = extendLoopWithCondition(b,shapeA,targetVar,getShapeCond(shapeB,a))
                (b.lhs zip loops2) foreach { p => t.subst(p._1) = p._2 }
                shapeA
              } else {
                assert(shapeA == shapeB, "loop shapes must match")
                shapeA
              }

              val fused = TTP(a.lhs:::b.lhs, SimpleFatLoop(shape, targetVar, WgetLoopRes(a):::WgetLoopRes(b)))
              partitionsOut = fused :: (partitionsOut diff List(a))
            case None => partitionsOut = b::partitionsOut
          }
        }
      
        printlog("partitions: " + partitionsOut)
      
      
        // actually do the fusion: now transform the loops bodies
      
        if ((partitionsOut intersect partitionsIn) != partitionsOut) { // was there any change?

          // within fused loops, remove accesses to outcomes of the fusion
          currentScope.foreach {
            case e@TTP(List(s), ThinDef(SimpleIndex(a, i))) =>
              printlog("considering " + e)
              partitionsOut.find(_.lhs contains a) match {
                case Some(fused) if WgetLoopVar(fused) contains t(i) => 
                  val index = fused.lhs.indexOf(a)
                  
                  printlog("replace " + e + " at " + index + " within " + fused)

                  val rhs = WgetLoopRes(fused)(index) match { case SimpleCollectIf(y,c) => y }
                  
                  t.subst(s) = rhs
                case _ => //e
              }
            case _ => //e
          }
          
          
          currentScope = getFatSchedule(currentScope)(currentScope) // clean things up!

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
          
          //Wloops = currentScope collect { case e @ TTP(_, FatLoop(_,_,_)) => e }

          Wloops = transformAll(partitionsOut, t)
          
          UloopSyms = UloopSyms map (t onlySyms _) // just lookup the symbols
          
          printlog("try once more ...")
        } else {
          printlog("no changes, we're done")
          done = true
        }
      
      } while (!done)
     
      
      // prune Wloops (some might be no longer necessary)
      Wloops = Wloops map {
        case TTP(lhs, SimpleFatLoop(s, x, rhs)) =>
          val ex = lhs map (s => currentScope exists (_.lhs == List(s)))
          def select[A](a: List[A], b: List[Boolean]) = (a zip b) collect { case (w, true) => w }
          TTP(select(lhs, ex), SimpleFatLoop(s, x, select(rhs, ex)))
      }
      
      // PREVIOUS PROBLEM: don't throw out all loops, might have some that are *not* in levelScope
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

    // the caller of emitBlock will quite likely call getBlockResult afterwards,
    // and if we change the result here, the caller will emit a reference to a sym
    // that doesn't exist (because it was replaced)

    if (result0 != result) {
      printlog("super.focusExactScopeFat with result changed from " + result0 + " to " + result)
      
      (result0 zip result) foreach {
        case (r0 @ Def(Reify(x, _, _)),Def(Reify(y, u, es))) => 
          if (!x.isInstanceOf[Sym[Any]])
            printlog("non-sym block result: " + x + " to " + y)
          else
            currentScope = currentScope :+ TTP(List(x.asInstanceOf[Sym[Any]]), ThinDef(Forward(y)))
          currentScope = currentScope :+ TTP(List(r0.asInstanceOf[Sym[Any]]), ThinDef(Reify(x,u,es)))
          // should rewire result so that x->y assignment is inserted
        case (r0,r) => 
          if (r0 != r) currentScope = currentScope :+ TTP(List(r0.asInstanceOf[Sym[Any]]), ThinDef(Forward(r)))
      }
      
    }

    // do what super does ...
    super.focusExactScopeFat(currentScope)(result0)(body)
  }

}
