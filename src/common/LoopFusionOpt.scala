package scala.virtualization.lms
package common

import internal.CodeMotion
import internal.Scheduling

/*
  current fusion algorithm:
  
    - start with a given scope, as obtained by focusBlock
    
    - find top level statements (using code motion algorithm)
    - get loops on top level, find out about inter-loop dependencies that would 
      prevent fusion (a(i) doesn't count because it can be eliminated from fused body)
    - build maximal sets of loops that can be fused
    - build up a substitution to unify loop variables and replace a(i) expressions in fused bodies.
      for filter-style loops, create new loop statements (parent shape + added condition), add to substituation
    - apply substitution to current scope (still contains the individual loop statements)
    - trim fused loop statements down using info from transformed scope (DCE, some loops gone away completely)
    - remove individual loop statements and replace them with fused statements
    
    - repeat until no further loops can be fused. then return modified scope.

  issue:
    
    - fusing loop bodies can remove references to loops: loops can become dead after they have been fused
    - if they are already part of a multi-loop statement, they will not be DCE'd (statement treated atomically)

    --> fusion needs to be optimistic about dce: assume no loop will be needed in the end
    
    --> possible way out: work with individual loop statements in current scope until converged, and 
        only then put in multi-loop stms
        (was similar to this before, but didn't take into account that code motion might make more
        loops available at top level in between runs (Test22))

  TODO:
    - better way to do DCE on multi-stms. 
    
    - previous model: graph has only TPs. TTPs are a purely codegen-time concept, created on demand
      - problem: keep TP and TTP version in sync
      - separate traversal / schedule classes for TP and TTP view (getSchedule vs getFatSchedule)
      - would like to commit to a TTP view sometimes (further transformations would like to work on multi loops)
    - now: TP and TTP both extend Stm, although TTPs are never created directly during staging
      - pro: single traversal classes (TTP based)
    
    
    - use case 1: loop fusion. xs.map(f).filter(p)
      - loops start out as individual loops, end up as multi loops

    - use case 2: if splitting.  var x = 2, y = 3; if (c) { x = 5; y = 7 }; println(y)
      - conditional starts out as multi if, which is split for each mutable var (to enable dce or detect invariant writes)

    - use case 3: loop splitting. var sum = 0; val acc = new ArrayBuffer(); (0 until 10) foreach { i => sum += f(i); acc append g(i) }
      - translate foreach loop into multi loop with an appropriate Elem (reduce, collect) 

    - use case 4: liszt style loop fission based on stencils. faces foreach { a => a.edges.foreach { b => table(b) += 6 } }
      - split loop into sequential phases (based on dependency information) where each phase can run in parallel

    
    
    - option 1: make fusion a transform phase that creates TTPs temporarily for traversal but  
      calls regular TP constructor methods to build transformed program 
      - how to? need to go down a multi loop body but create single loop stms as result of transformation.
      - challenge: parallelization needs to look at all fused loops to determine whether a reduce or scan step is necessary.
    
    - idea: never make TTP part of graph, only use as an abstraction in codegen (previous model).
      but: keep info which stms should be fused around in a separate table.
      - challenge: keep extra table up to date when things are transformed
      
    - getSchedule: first pass to determine needed statements, second pass to define order and merging,
      taking anti-dependence and fusion information into account

*/


/*
  unrelated question: can we separate loop fusion and code motion?
  
    a1 = SimpleLoop(s, i1, ArrayElem(Block(e1)))

    a2 = SimpleLoop(s, i2, ArrayElem(Block(e2)))
  
  becomes:
  
    a1,a2 = SimpleFatLoop(s, i12, ArrayElem(Block(e1)), ArrayElem(Block(e2)))

  we still have separate blocks for the ArrayElems.
  
  currently, calling emitFatBlock on the list of all embedded blocks List(e1,e2)
  will effectively combine the blocks during the descent, triggering
  fusion of inner loops and other optimizations.
  
  if we descend separately we miss opportunities.

  an isolated fusion pass will need to physically merge the blocks. a yield
  based version may achieve this:

    a1,a2 = SimpleFatLoop(s, i12, y1=ArrayElem, y2=ArrayElem, Block(e12)))
  
  where e12 invokes y1 and y2 internally.
  
  RULE: isolated passes need to merge blocks
*/


/*
  another idea: make block results staging time values
  
    case class Block[T](res: T)
    
  and use syms(res) to get the block result expressions.
  
  this would allow moving arbitrary stuff inside blocks:
  
    a1,a2 = SimpleFatLoop(s, i12, Block((ArrayElem(e1), ArrayElem(e2))))

*/


/*
  scheduling/dce/code motion: really a few separate things
  
    1) available: get all upward dependencies from result (backwards,dce,available)
    2) from that set, get all downward dependencies on bound variables, up to the node in which the var is bound (forward,must-below)
  
*/



/*
  another unrelated question: is mirroring always behaving correctly?
  what are the side conditions?
  
  RULE: mirror should not create new syms (apart from final result via toAtom)
        we must not create a new sym for one that is to be replaced
  RULE: mirror should not not call reifyEffects 
        why? it will likely call reflectEffect inside, which will create new syms. 
        maybe we already have a replacement for one of the effectful nodes.
  
  if mirror has a replacement for a node, apply it. if inputs of replacement
  are in domain of substitution, mirror replacement.
  
  mirroring is context-free in general. (for Reflect nodes we do setup a 
  dummy context with the tranformed inputs)
  
  
  thinking about a test case where mirroring breaks:
  
    val x = mzeros(100)

    val p = any()

    val a = foobar(x,i,p)

  then go ahead and replace p -> Const(0)

    def foobar(x,i,p) = p match {
      case Const(0) => 
        
        // something effectful...
        vapply(x,i)
        
      case _ => FooBar(x,i,p)
    }

    val a = vapply(x,i)   // becomes Reflect(VectorApply(x,i), Read(x))

  not sure...
  
*/



trait LoopFusionOpt extends internal.FatBlockTraversal with LoopFusionCore {
  val IR: LoopsFatExp with IfThenElseFatExp
  import IR._  

  

  override def focusExactScopeFat[A](resultB: List[Block[Any]])(body: List[Stm] => A): A = {
    val result0 = resultB.map(getBlockResultFull) flatMap { case Combine(xs) => xs case x => List(x) }
    val (scope,result) = fuseTopLevelLoops(innerScope)(result0)
    innerScope = scope

    // we don't currently propagate a modified result to the parent

    // the caller of traverseBlock will quite likely call getBlockResult afterwards,
    // and if we change the result here, the caller will emit a reference to a sym
    // that doesn't exist (because it was replaced)

    if (result0 != result) {
      printlog("super.focusExactScopeFat with result changed from " + result0 + " to " + result)

      (result0 zip result) foreach {
        case (r0 @ Def(Reify(x, _, _)),Def(Reify(y, u, es))) => 
          if (!x.isInstanceOf[Sym[Any]])
            printlog("non-sym block result: " + x + " to " + y)
          else if (x != y)
            innerScope = innerScope :+ TP(x.asInstanceOf[Sym[Any]], Forward(y))
          innerScope = innerScope :+ TP(r0.asInstanceOf[Sym[Any]], Reify(x,u,es))
          // should rewire result so that x->y assignment is inserted
        case (r0,r) => 
          if (r0 != r) innerScope = innerScope :+ TP(r0.asInstanceOf[Sym[Any]], Forward(r))
      }
    }

    super.focusExactScopeFat(result0.map(Block(_)))(body)
  }
  

}



trait LoopFusionCore extends internal.FatScheduling with CodeMotion with SimplifyTransform {
  val IR: LoopsFatExp with IfThenElseFatExp
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


  /*
    apply fusion to loops at the top level of scope 'currentScope', which has outputs 'result'.
    uses 'getExactScope' provided by CodeMotion to find top level statements.
    returns updated scope and results.
  */

  def fuseTopLevelLoops(currentScope0: List[Stm])(result0: List[Exp[Any]]): (List[Stm], List[Exp[Any]]) = {
    var result: List[Exp[Any]] = result0
    var currentScope: List[Stm] = currentScope0

    if (!shouldApplyFusion(currentScope)(result))
      return (currentScope, result)
/*
    println("--- pre-pre-loop fusion: bound")
    val bound = currentScope.flatMap(z => boundSyms(z.rhs))
    bound.foreach(println)

    println("--- pre-pre-loop fusion: dependent on bound")
    val g1 = getFatDependentStuff(currentScope)(bound)
    g1.foreach(println)
*/

    // find loops at current top level
    var Wloops: List[Stm] = {
      val levelScope = getExactScope(currentScope)(result) // could provide as input ...
      // TODO: cannot in general fuse several effect loops (one effectful and several pure ones is ok though)
      // so we need a strategy. a simple one would be exclude all effectful loops right away (TODO).
      levelScope collect { case e @ TTP(_, _, SimpleFatLoop(_,_,_)) => e }
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
      var UloopSyms = currentScope collect { case e @ TTP(lhs, _, SimpleFatLoop(_,_,_)) if !Wloops.contains(e) => lhs }

      //do{
        
        // utils
        def WgetLoopShape(e: Stm): Exp[Int] = e.rhs match { case SimpleFatLoop(s,x,rhs) => s }
        def WgetLoopVar(e: Stm): List[Sym[Int]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => List(x) }
        def WgetLoopRes(e: Stm): List[Def[Any]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => rhs }

        val loopCollectSyms = Wloops flatMap (e => (e.lhs zip WgetLoopRes(e)) collect { case (s, SimpleCollectIf(_,_)) => s })

        val loopSyms = Wloops flatMap (_.lhs)
        val loopVars = Wloops flatMap WgetLoopVar

        val WloopSyms = Wloops map (_.lhs)
        val WloopVars = Wloops map WgetLoopVar

        //println("Wloops " + Wloops)

        // find negative dependencies (those that block fusion)

        // might be costly: resolve and traverse complete input deps for each loop body
        // O(nloops*currentScope.length), assuming schedule is linear (NOT true currently!)

        // possible extension: have WtableNeg keep track of the statements that prevent fusion
        // then we can later remove the entry and see if the dependency goes away...

        var WtableNeg = Wloops.flatMap { dx => // find non-simple dependencies (other than a(i))
          val thisLoopVars = WgetLoopVar(dx)
          val otherLoopSyms = loopSyms diff (dx.lhs)
          getSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
            case e@TP(_, SimpleIndex(a,i)) if (thisLoopVars contains i) && (loopCollectSyms contains a) => 
              Nil // direct deps on this loop's induction var don't count
            case sc =>
              val pr = syms(sc.rhs).intersect(otherLoopSyms) flatMap { otherLoop => dx.lhs map ((otherLoop, _)) }
              if (pr.nonEmpty) printlog("fusion of "+pr+" prevented by " + sc + " which is required by body of " + dx.lhs)// + " / " + thisLoopVars)
              pr
          }
        }.distinct      

        // transitive closure of negative dependencies
        def iter {
          val oldNeg = WtableNeg
          val delta = WtableNeg flatMap { p => WtableNeg collect { case q if p._2 == q._1 => (p._1, q._2) }}
          WtableNeg = (WtableNeg ++ delta).distinct
          if (WtableNeg != oldNeg) iter
        }
        iter

        printlog("wtableneg: " + WtableNeg) // will add more later, need to maintain closure


        // other preconditions for fusion: loops must have same shape, or one must loop over the other's result

        def canFuseIndirect(a: Stm, b: Stm): Boolean = 
          !WtableNeg.exists(p => (a.lhs contains p._1) && (b.lhs contains p._2) || (b.lhs contains p._1) && (a.lhs contains p._2))

        def canFuseDirect(a: Stm, b: Stm): Boolean = (a.rhs,b.rhs) match {
          case (SimpleFatLoop(s1,_,_), SimpleFatLoop(s2,_,_)) if s1 == s2 => true  // same size (horizontal or pipeline)
          case (SimpleFatLoop(Def(SimpleDomain(a1)),_,_), SimpleFatLoop(_,_,_)) if b.lhs contains a1 => true // pipeline
          case (SimpleFatLoop(_,_,_), SimpleFatLoop(Def(SimpleDomain(b1)),_,_)) if a.lhs contains b1 => true
          case _ => false
        }

        def canFuse(a: Stm, b: Stm): Boolean = canFuseDirect(a,b) && canFuseIndirect(a,b)        

        // shape dependency helpers

        def isShapeDep(s: Exp[Int], a: Stm) = s match { case Def(SimpleDomain(a1)) => a.lhs contains a1 case _ => false }
        def getShapeCond(s: Exp[Int], a: Stm) = s match { case Def(SimpleDomain(a1)) => WgetLoopRes(a)(a.lhs indexOf a1) match { case SimpleCollectIf(a,c) => c } }

        def extendLoopWithCondition(e: Stm, shape: Exp[Int], targetVar: Sym[Int], c: List[Exp[Boolean]]): List[Exp[Any]] = e.rhs match { 
          case SimpleFatLoop(s,x,rhs) => (e.lhs zip rhs).map { case (l,r) => findOrCreateDefinitionExp(SimpleLoop(shape,targetVar,applyAddCondition(r,c)), l.pos) }
        }

        // partitioning: build maximal sets of loops to be fused
        // already fuse loops headers (shape, index variables)

        val t = new SubstTransformer

        var partitionsIn = Wloops
        var partitionsOut = Nil:List[Stm]

        for (b@ TTP(_,_,_) <- partitionsIn) {
          // try to add to an item in partitionsOut, if not possible add as-is
          partitionsOut.find(a => canFuse(a,b)) match {
            case Some(a: TTP) => 

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

              val lhs = a.lhs ++ b.lhs

              val fused = TTP(lhs, a.mhs ++ b.mhs, SimpleFatLoop(shape, targetVar, WgetLoopRes(a):::WgetLoopRes(b)))
              partitionsOut = fused :: (partitionsOut diff List(a))

              val preNeg = WtableNeg collect { case p if (lhs contains p._2) => p._1 }
              val postNeg = WtableNeg collect { case p if (lhs contains p._1) => p._2 }

              val fusedNeg = preNeg flatMap { s1 => postNeg map { s2 => (s1,s2) } }
              WtableNeg = (fusedNeg ++ WtableNeg).distinct

            case None => partitionsOut = b::partitionsOut
          }
        }

        //printlog("partitionsIn: " + partitionsIn)
        printlog("partitions: " + partitionsOut)

        // actually do the fusion: now transform the loops bodies

        if ((partitionsOut intersect partitionsIn) != partitionsOut) { // was there any change?

          // within fused loops, remove accesses to outcomes of the fusion
          currentScope.foreach {
            case e@TP(s, SimpleIndex(a, i)) => // if a is a loop result, i is a loop var, and both loops are fused
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

          // ---

          //currentScope = (currentScope diff partitionsIn) ::: partitionsOut
          /*println("<---0")
          currentScope foreach println
          println("--->0")*/

          // go ahead and apply accumulated substitution
          transformAllFully(currentScope, result, t) match { case (a,b) => // too bad we can't use pair assigment
            currentScope = a
            result = b
          }

          /*println("<---1")
          currentScope foreach println
          println("--->1")*/
          
          // this needs to do DCE ...
          
          val pInT = transformAll(partitionsIn, t)
          val pOutT = transformAll(partitionsOut, t)

          //println("pInT " + pInT)
          //println("pOutT " + pOutT)

          // prune Wloops (some might be no longer necessary)
          Wloops = pOutT map {
            case TTP(lhs, mhs, SimpleFatLoop(s, x, rhs)) =>
              val ex = lhs map (s => currentScope exists (_.lhs contains s))
              def select[A](a: List[A], b: List[Boolean]) = (a zip b) collect { case (w, true) => w }
              TTP(select(lhs, ex), select(mhs, ex), SimpleFatLoop(s, x, select(rhs, ex)))
          }

          currentScope = (currentScope diff pInT) ++ Wloops

          // schedule
          currentScope = getSchedule(currentScope)(result) // get the order right


          /*println("<---2")
          currentScope foreach println
          println("--->2")*/

          printlog("try once more ...")

          return fuseTopLevelLoops(currentScope)(result)
        }
        printlog("no changes, we're done")
    }

/*
    println("result "+result0+"/"+result)
    println("<---")
    currentScope.foreach(println)
    println("--->")
*/
    (currentScope, result)
  }







/*  def fuseTopLevelLoops(currentScope0: List[Stm])(result0: List[Exp[Any]]): (List[Stm], List[Exp[Any]]) = {
    var result: List[Exp[Any]] = result0
    var currentScope: List[Stm] = currentScope0

    if (!shouldApplyFusion(currentScope)(result))
      return (currentScope, result)
/*
    println("--- pre-pre-loop fusion: bound")
    val bound = currentScope.flatMap(z => boundSyms(z.rhs))
    bound.foreach(println)

    println("--- pre-pre-loop fusion: dependent on bound")
    val g1 = getFatDependentStuff(currentScope)(bound)
    g1.foreach(println)
*/

    // find loops at current top level
    var Wloops: List[Stm] = {
      val levelScope = getExactScope(currentScope)(result) // could provide as input ...
      // TODO: cannot in general fuse several effect loops (one effectful and several pure ones is ok though)
      // so we need a strategy. a simple one would be exclude all effectful loops right away (TODO).
      levelScope collect { case e @ TTP(_, _, SimpleFatLoop(_,_,_)) => e }
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
      var UloopSyms = currentScope collect { case e @ TTP(lhs, _, SimpleFatLoop(_,_,_)) if !Wloops.contains(e) => lhs }
      
      do {
        // utils
        def WgetLoopShape(e: Stm): Exp[Int] = e.rhs match { case SimpleFatLoop(s,x,rhs) => s }
        def WgetLoopVar(e: Stm): List[Sym[Int]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => List(x) }
        def WgetLoopRes(e: Stm): List[Def[Any]] = e.rhs match { case SimpleFatLoop(s,x,rhs) => rhs }

        val loopCollectSyms = Wloops flatMap (e => (e.lhs zip WgetLoopRes(e)) collect { case (s, SimpleCollectIf(_,_)) => s })
        
        val loopSyms = Wloops flatMap (_.lhs)
        val loopVars = Wloops flatMap WgetLoopVar

        val WloopSyms = Wloops map (_.lhs)
        val WloopVars = Wloops map WgetLoopVar

    println("Wloops " + Wloops)
    
        // find negative dependencies (those that block fusion)
        
        // might be costly: resolve and traverse complete input deps for each loop body
        // O(nloops*currentScope.length), assuming schedule is linear (NOT true currently!)

        // possible extension: have WtableNeg keep track of the statements that prevent fusion
        // then we can later remove the entry and see if the dependency goes away...
        
        var WtableNeg = Wloops.flatMap { dx => // find non-simple dependencies (other than a(i))
          val thisLoopVars = WgetLoopVar(dx)
          val otherLoopSyms = loopSyms diff (dx.lhs)
/*
          getSchedule(currentScope)(WgetLoopRes(dx)) flatMap {
            case e@TP(_, SimpleIndex(a,i)) if (thisLoopVars contains i) && (loopCollectSyms contains a) => 
*/
          getCustomFatSchedule(currentScope)(WgetLoopRes(dx)) {
            case e@ThinDef(SimpleIndex(a,i)) if (thisLoopVars contains i) && (loopCollectSyms contains a) => 
              // check that a is the result of a SimpleCollectIf loop (not a reduce, for example)
              //if (!loopCollectSyms.contains(a))
              //  printerr("DANGER WILL ROBINSON: ignoring dep " + e + " although " + a + " is not a loop sym " + loopCollectSyms)
              printdbg("ignoring simple dependency " + e + " on loop var " + thisLoopVars + " required by body of " + dx.lhs)
              Nil // direct deps on this loop's induction var don't count
            case e => 
              syms(e)
          } flatMap {
            case e@TTP(_, ThinDef(SimpleIndex(a,i))) if (thisLoopVars contains i) && (loopCollectSyms contains a) =>
              printdbg("ignoring2 simple dependency " + e + " on loop var " + thisLoopVars + " required by body of " + dx.lhs)
              Nil //FIXME: shouldn't duplicate condition ...
            case sc =>
              val pr = syms(sc.rhs).intersect(otherLoopSyms) flatMap { otherLoop => dx.lhs map ((otherLoop, _)) }
              if (pr.nonEmpty) printlog("fusion of "+pr+" prevented by " + sc + " which is required by body of " + dx.lhs + " / " + thisLoopVars)
              pr
          }
        }.distinct      
      
        // transitive closure of negative dependencies
        def iter {
          val oldNeg = WtableNeg
          val delta = WtableNeg flatMap { p => WtableNeg collect { case q if p._2 == q._1 => (p._1, q._2) }}
          WtableNeg = (WtableNeg ++ delta).distinct
          if (WtableNeg != oldNeg) iter
        }
        iter
      
        printlog("wtableneg: " + WtableNeg) // will add more later, need to maintain closure
        
        // other preconditions for fusion: loops must have same shape, or one must loop over the other's result
        
        def canFuseIndirect(a: Stm, b: Stm): Boolean = 
          !WtableNeg.exists(p => (a.lhs contains p._1) && (b.lhs contains p._2) || (b.lhs contains p._1) && (a.lhs contains p._2))
        
        def canFuseDirect(a: Stm, b: Stm): Boolean = (a.rhs,b.rhs) match {
          case (SimpleFatLoop(s1,_,_), SimpleFatLoop(s2,_,_)) if s1 == s2 => true  // same size (horizontal or pipeline)
          case (SimpleFatLoop(Def(SimpleDomain(a1)),_,_), SimpleFatLoop(_,_,_)) if b.lhs contains a1 => true // pipeline
          case (SimpleFatLoop(_,_,_), SimpleFatLoop(Def(SimpleDomain(b1)),_,_)) if a.lhs contains b1 => true
          case _ => false
        }
        
        def canFuse(a: Stm, b: Stm): Boolean = canFuseDirect(a,b) && canFuseIndirect(a,b)        
        
        // shape dependency helpers
        
        def isShapeDep(s: Exp[Int], a: Stm) = s match { case Def(SimpleDomain(a1)) => a.lhs contains a1 case _ => false }
        def getShapeCond(s: Exp[Int], a: Stm) = s match { case Def(SimpleDomain(a1)) => WgetLoopRes(a)(a.lhs indexOf a1) match { case SimpleCollectIf(a,c) => c } }
        
        def extendLoopWithCondition(e: Stm, shape: Exp[Int], targetVar: Sym[Int], c: List[Exp[Boolean]]): List[Exp[Any]] = e.rhs match { 
          case SimpleFatLoop(s,x,rhs) => (e.lhs zip rhs).map { case (l,r) => findOrCreateDefinitionExp(SimpleLoop(shape,targetVar,applyAddCondition(r,c)), l.pos) }
        }
        
        // partitioning: build maximal sets of loops to be fused
        // already fuse loops headers (shape, index variables)
        
        val t = new SubstTransformer

        var partitionsIn = Wloops
        var partitionsOut = Nil:List[Stm]
      
        for (b@ TTP(_,_,_) <- partitionsIn) {
          // try to add to an item in partitionsOut, if not possible add as-is
          partitionsOut.find(a => canFuse(a,b)) match {
            case Some(a: TTP) => 
              
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

              val lhs = a.lhs ++ b.lhs

              val fused = TTP(lhs, a.mhs ++ b.mhs, SimpleFatLoop(shape, targetVar, WgetLoopRes(a):::WgetLoopRes(b)))
              partitionsOut = fused :: (partitionsOut diff List(a))

              val preNeg = WtableNeg collect { case p if (lhs contains p._2) => p._1 }
              val postNeg = WtableNeg collect { case p if (lhs contains p._1) => p._2 }
              
              val fusedNeg = preNeg flatMap { s1 => postNeg map { s2 => (s1,s2) } }
              WtableNeg = (fusedNeg ++ WtableNeg).distinct

            case None => partitionsOut = b::partitionsOut
          }
        }
      
        printlog("partitions: " + partitionsOut)
      
        // actually do the fusion: now transform the loops bodies
      
        if ((partitionsOut intersect partitionsIn) != partitionsOut) { // was there any change?

          // within fused loops, remove accesses to outcomes of the fusion
          currentScope.foreach {
            case e@TP(s, SimpleIndex(a, i)) => // if a is a loop result, i is a loop var, and both loops are fused
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
          
          // ---
          
          // go ahead and apply accumulated substitution
          
          transformAllFully(currentScope, result, t) match { case (a,b) => // too bad we can't use pair assigment
            currentScope = a
            result = b
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
        case TTP(lhs, mhs, SimpleFatLoop(s, x, rhs)) =>
          val ex = lhs map (s => currentScope exists (_.lhs == List(s)))
          def select[A](a: List[A], b: List[Boolean]) = (a zip b) collect { case (w, true) => w }
          TTP(select(lhs, ex), select(mhs, ex), SimpleFatLoop(s, x, select(rhs, ex)))
      }
      
      // PREVIOUS PROBLEM: don't throw out all loops, might have some that are *not* in levelScope
      // note: if we don't do it here, we will likely see a problem going back to innerScope in 
      // FatCodegen.focusExactScopeFat below. --> how to go back from SimpleFatLoop to VectorPlus??
      // UPDATE: UloopSyms puts a tentative fix in place. check if it is sufficient!!
      // what is the reason we cannot just look at Wloops??
      currentScope = currentScope.filter { case e@TTP(lhs, _, _: AbstractFatLoop) => 
        val keep = UloopSyms contains lhs
        //if (!keep) println("dropping: " + e + ", not int UloopSyms: " + UloopSyms)
        keep case _ => true } ::: Wloops

      // schedule
      currentScope = getSchedule(currentScope)(result) // clean things up!
    }

/*
    println("result "+result0+"/"+result)
    println("<---")
    currentScope.foreach(println)
    println("--->")
*/
    (currentScope, result)
  }*/

}
