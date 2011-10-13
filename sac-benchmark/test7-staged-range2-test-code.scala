package scala.virtualization.lms
package epfl
package test7
package original

import test7.original.Conversions._
import test7.original.Operations._
import test7.original.SpecificOperations._

/*****************************************
  Emitting Generated Code                  
*******************************************/
class Experiment extends ((scala.virtualization.lms.epfl.test7.original.MDArray[Double])=>(scala.virtualization.lms.epfl.test7.original.MDArray[Double])) {
  def apply(x87:scala.virtualization.lms.epfl.test7.original.MDArray[Double]): scala.virtualization.lms.epfl.test7.original.MDArray[Double] = {
    // RuntimeCheck : POST:   S87 = [u382  u381  u380]                               from Bubble up shape for Sym(87) <- Shape(Sym(87))
    // Shape: V88=[u382  u381  u380] and S88=[3]
    val x88: MDArray[Int] = shape(x87)
    // RuntimeCheck : POST:   V88 = [u382  u381  u380]                               from Bubble up value for Sym(88) <- GenArrayWith(Sym(88) - Sym(94))
    // RuntimeCheck : POST:   S88 = [3]                                              from Bubble up shape for Sym(88) <- GenArrayWith(Sym(88) - Sym(94))
    // RuntimeCheck : POST:   V94 = [u313]                                           from Bubble up value for Sym(94) <- GenArrayWith(Sym(88) - Sym(94))
    // RuntimeCheck : POST:   S94 = []                                               from Bubble up shape for Sym(94) <- GenArrayWith(Sym(88) - Sym(94))
    // RuntimeCheck : PRE:    S88 = [u11053]                                         from GenArrayWith(Sym(88) - Sym(94))
    // RuntimeCheck : PRE:    S88 = S2                                               from GenArrayWith(Sym(88) - Sym(94))
    // RuntimeCheck : PRE:    V88(:length(V2)) < V2                                  from GenArrayWith(Sym(88) - Sym(94))
    // RuntimeCheck : PRE:    V88(length(V2):) = S94                                 from GenArrayWith(Sym(88) - Sym(94))
    // Shape: S95=[u382  u381  u380]
    
    val x95: MDArray[Boolean] = {
      val opName: String = "genarray"
      var result: Array[Boolean] = null
      var rshape: Array[Int] = null
      // Shape: V2=[1  0  0] and S2=[3]
      val x2: MDArray[Int] = internalReshape(3::Nil, Array(1, 0, 0), "knownAtCompileTime")
      // Shape: V3=[2  1  1] and S3=[3]
      val x3: MDArray[Int] = internalReshape(3::Nil, Array(2, 1, 1), "knownAtCompileTime")
      // Shape: V4=[u2] and S4=[]
      val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
      // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
      // Shape: V5=[u230] and S5=[]
      val x5: Boolean = x4
      // RuntimeCheck : POST:   V5 = [u230]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
      // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
      // Shape: V14=[u229] and S14=[]
      val x14: Boolean = x5
      // RuntimeCheck : POST:   V14 = [u229]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
      // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
      // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
      // Shape: V15=[u228] and S15=[]
      val x15: Boolean = x14
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(88) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(88) and Sym(7))
      // RuntimeCheck : POST:   V88 = [u382  u381  u380]                               from Bubble up value for Sym(88) <- InfixOp(-: Sym(88) and Sym(7))
      // RuntimeCheck : POST:   S88 = [3]                                              from Bubble up shape for Sym(88) <- InfixOp(-: Sym(88) and Sym(7))
      // RuntimeCheck : PRE:    S88 = S7 OR S7 = []                                    from InfixOp(-: Sym(88) and Sym(7))
      // Shape: V89=[u429  u428  u427] and S89=[3]
      val x89: MDArray[Int] = {
        val result = new Array[Int](shape(x88).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x88.content()(i) -  x7
        internalReshape(shape(x88), result, "infixOpAA")
      }
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V88 = [u382  u381  u380]                               from Bubble up value for Sym(88) <- Shape(Sym(88))
      // RuntimeCheck : POST:   S88 = [3]                                              from Bubble up shape for Sym(88) <- Shape(Sym(88))
      // Shape: V90=[3] and S90=[1]
      val x90: MDArray[Int] = shape(x88)
      // RuntimeCheck : POST:   V90 = [3]                                              from Bubble up value for Sym(90) <- Sel(Sym(9), Sym(90))
      // RuntimeCheck : POST:   S90 = [1]                                              from Bubble up shape for Sym(90) <- Sel(Sym(9), Sym(90))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(90))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(90))
      // RuntimeCheck : PRE:    length(S9) = length([u11074])                          from Sel(Sym(9), Sym(90))
      // RuntimeCheck : PRE:    S90(:length(V9)) < V9                                  from Sel(Sym(9), Sym(90))
      // Shape: V91=[3] and S91=[]
      
      // Shape: V91=[3] and S91=[]
      val x91: Int = x90.content()(flatten(shape(x90), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(91))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(91))
      // RuntimeCheck : POST:   V91 = [3]                                              from Bubble up value for Sym(91) <- Values(Sym(12), Sym(91))
      // RuntimeCheck : POST:   S91 = []                                               from Bubble up shape for Sym(91) <- Values(Sym(12), Sym(91))
      // RuntimeCheck : PRE:    S91 = []                                               from Values(Sym(12), Sym(91))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(91))
      // Shape: V92=[0  0  0] and S92=[3]
      val x92: MDArray[Int] = {
        val result = new Array[Int](x91)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x91::Nil, result, "values")
      }
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   S93 = [3]                                              from Bubble up shape for Sym(93) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   V92 = [0  0  0]                                        from Bubble up value for Sym(92) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   S92 = [3]                                              from Bubble up shape for Sym(92) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   V3 = [2  1  1]                                         from Bubble up value for Sym(3) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   S3 = [3]                                               from Bubble up shape for Sym(3) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   V15 = [u228]                                           from Bubble up value for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   V89 = [u429  u428  u427]                               from Bubble up value for Sym(89) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   S89 = [3]                                              from Bubble up shape for Sym(89) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   V15 = [u228]                                           from Bubble up value for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   V2 = [1  0  0]                                         from Bubble up value for Sym(2) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : POST:   S2 = [3]                                               from Bubble up shape for Sym(2) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : PRE:    length(S2) = length([u11054])                          from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : PRE:    S89 = S2                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : PRE:    S3 = S2                                                from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : PRE:    S92 = S2                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // RuntimeCheck : PRE:    V2 < V89                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      // Shape: V94=[u313] and S94=[]
      // with: With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(89) step=Sym(3) width=Sym(92)  Sym(93) => Sym(17))
      val lb0: Int = x2.content()(0)
      val ub0: Int = x89.content()(0)
      val step0: Int = x3.content()(0)
      val width0: Int = x92.content()(0)
      val ll0: Int = if (x15) lb0 + 1 else lb0
      val ul0: Int = if (x15) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x2.content()(1)
          val ub1: Int = x89.content()(1)
          val step1: Int = x3.content()(1)
          val width1: Int = x92.content()(1)
          val ll1: Int = if (x15) lb1 + 1 else lb1
          val ul1: Int = if (x15) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x2.content()(2)
              val ub2: Int = x89.content()(2)
              val step2: Int = x3.content()(2)
              val width2: Int = x92.content()(2)
              val ll2: Int = if (x15) lb2 + 1 else lb2
              val ul2: Int = if (x15) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x93: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x93
                  val feval: MDArray[Boolean] = {
                    x17
                  }
                  // the action of this loop:
                  if (result == null) {
                    // create the array and shape
                    result = new Array[Boolean](x88.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                    rshape = shape(feval).content()
                  } else {
                    // check shape -- this WILL be redundant due to runtime checks
                    if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                  }
                  // copy new content
                  val mainIndex: Int = flatten(x88 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                  for (innerIndex <- List.range(0, rshape.length)) {
                    result(mainIndex + innerIndex) = feval(innerIndex)
                  }
                } // if ((iv0 ...
              } // for (iv0 ...
            } // if ((iv1 ...
          } // for (iv1 ...
        } // if ((iv2 ...
      } // for (iv2 ...
      internalReshape(x88 ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S87 = [u382  u381  u380]                               from Bubble up shape for Sym(87) <- ModArrayWith(Sym(87) - Sym(137))
    // RuntimeCheck : POST:   V137 = [u456]                                          from Bubble up value for Sym(137) <- ModArrayWith(Sym(87) - Sym(137))
    // RuntimeCheck : POST:   S137 = []                                              from Bubble up shape for Sym(137) <- ModArrayWith(Sym(87) - Sym(137))
    // RuntimeCheck : PRE:    S92 = [u11063]                                         from ModArrayWith(Sym(87) - Sym(137))
    // RuntimeCheck : PRE:    S92 = [LengthOf(S87)]                                  from ModArrayWith(Sym(87) - Sym(137))
    // RuntimeCheck : PRE:    S87(:length(V92)) < V92                                from ModArrayWith(Sym(87) - Sym(137))
    // RuntimeCheck : PRE:    S87(length(V92):) = S137                               from ModArrayWith(Sym(87) - Sym(137))
    // Shape: S138=[u382  u381  u380]
    
    val x138: MDArray[Double] = {
      val opName: String = "modarray"
      var result: Array[Double] = new Array[Double](shape(x87).content().foldLeft(1)((a,b) => a*b))
      for (i <- List.range(0, result.length)) {
        result(i) = x87.content()(i)
      }
      var rshape: List[Int] = null
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V88 = [u382  u381  u380]                               from Bubble up value for Sym(88) <- Shape(Sym(88))
      // RuntimeCheck : POST:   S88 = [3]                                              from Bubble up shape for Sym(88) <- Shape(Sym(88))
      // Shape: V90=[3] and S90=[1]
      val x90: MDArray[Int] = shape(x88)
      // RuntimeCheck : POST:   V90 = [3]                                              from Bubble up value for Sym(90) <- Sel(Sym(9), Sym(90))
      // RuntimeCheck : POST:   S90 = [1]                                              from Bubble up shape for Sym(90) <- Sel(Sym(9), Sym(90))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(90))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(90))
      // RuntimeCheck : PRE:    length(S9) = length([u11074])                          from Sel(Sym(9), Sym(90))
      // RuntimeCheck : PRE:    S90(:length(V9)) < V9                                  from Sel(Sym(9), Sym(90))
      // Shape: V91=[3] and S91=[]
      
      // Shape: V91=[3] and S91=[]
      val x91: Int = x90.content()(flatten(shape(x90), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(91))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(91))
      // RuntimeCheck : POST:   V91 = [3]                                              from Bubble up value for Sym(91) <- Values(Sym(12), Sym(91))
      // RuntimeCheck : POST:   S91 = []                                               from Bubble up shape for Sym(91) <- Values(Sym(12), Sym(91))
      // RuntimeCheck : PRE:    S91 = []                                               from Values(Sym(12), Sym(91))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(91))
      // Shape: V92=[0  0  0] and S92=[3]
      val x92: MDArray[Int] = {
        val result = new Array[Int](x91)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x91::Nil, result, "values")
      }
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : PRE:    length(S17) = length([])                               from ToValue(Sym(17))
      // Shape: V21=[u250] and S21=[]
      val x21: Boolean = x17
      // RuntimeCheck : POST:   V21 = [u250]                                           from Bubble up value for Sym(21) <- FromValue(Sym(21))
      // RuntimeCheck : POST:   S21 = []                                               from Bubble up shape for Sym(21) <- FromValue(Sym(21))
      // Shape: V24=[u249] and S24=[]
      val x24: Boolean = x21
      // RuntimeCheck : POST:   V24 = [u249]                                           from Bubble up value for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : PRE:    length(S24) = length([])                               from ToValue(Sym(24))
      // Shape: V25=[u248] and S25=[]
      val x25: Boolean = x24
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(88) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(88) and Sym(7))
      // RuntimeCheck : POST:   V88 = [u382  u381  u380]                               from Bubble up value for Sym(88) <- InfixOp(-: Sym(88) and Sym(7))
      // RuntimeCheck : POST:   S88 = [3]                                              from Bubble up shape for Sym(88) <- InfixOp(-: Sym(88) and Sym(7))
      // RuntimeCheck : PRE:    S88 = S7 OR S7 = []                                    from InfixOp(-: Sym(88) and Sym(7))
      // Shape: V99=[u426  u425  u424] and S99=[3]
      val x99: MDArray[Int] = {
        val result = new Array[Int](shape(x88).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x88.content()(i) -  x7
        internalReshape(shape(x88), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(91))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(91))
      // RuntimeCheck : POST:   V91 = [3]                                              from Bubble up value for Sym(91) <- Values(Sym(7), Sym(91))
      // RuntimeCheck : POST:   S91 = []                                               from Bubble up shape for Sym(91) <- Values(Sym(7), Sym(91))
      // RuntimeCheck : PRE:    S91 = []                                               from Values(Sym(7), Sym(91))
      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(91))
      // Shape: V100=[1  1  1] and S100=[3]
      val x100: MDArray[Int] = {
        val result = new Array[Int](x91)
        for(i <- List.range(0, result.length))
        result(i) = x7
        internalReshape(x91::Nil, result, "values")
      }
      // Shape: V49=[u11] and S49=[]
      val x49: Double = internalReshape(Nil, Array(0.16666666666666666), "knownAtCompileTime")
      // Shape: V20=[u12] and S20=[]
      val x20: Double = internalReshape(Nil, Array(0.1), "knownAtCompileTime")
      // Shape: V115=[u13] and S115=[]
      val x115: Double = internalReshape(Nil, Array(0.0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V136 = [u457]                                          from Bubble up value for Sym(136) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   S136 = []                                              from Bubble up shape for Sym(136) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   V101 = [u487(<u382)  u488(<u381)  u489(<u380)]         from Bubble up value for Sym(101) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   S101 = [3]                                             from Bubble up shape for Sym(101) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   V92 = [0  0  0]                                        from Bubble up value for Sym(92) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   S92 = [3]                                              from Bubble up shape for Sym(92) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   V100 = [1  1  1]                                       from Bubble up value for Sym(100) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   S100 = [3]                                             from Bubble up shape for Sym(100) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   V25 = [u248]                                           from Bubble up value for Sym(25) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   V99 = [u426  u425  u424]                               from Bubble up value for Sym(99) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   S99 = [3]                                              from Bubble up shape for Sym(99) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   V25 = [u248]                                           from Bubble up value for Sym(25) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   V92 = [0  0  0]                                        from Bubble up value for Sym(92) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : POST:   S92 = [3]                                              from Bubble up shape for Sym(92) <- With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : PRE:    length(S92) = length([u11064])                         from With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : PRE:    S99 = S92                                              from With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : PRE:    S100 = S92                                             from With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : PRE:    S92 = S92                                              from With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // RuntimeCheck : PRE:    V92 < V99                                              from With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      // Shape: V137=[u456] and S137=[]
      // with: With(lb=Sym(92) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(99) step=Sym(100) width=Sym(92)  Sym(101) => Sym(136))
      val lb0: Int = x92.content()(0)
      val ub0: Int = x99.content()(0)
      val step0: Int = x100.content()(0)
      val width0: Int = x92.content()(0)
      val ll0: Int = if (x25) lb0 + 1 else lb0
      val ul0: Int = if (x25) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x92.content()(1)
          val ub1: Int = x99.content()(1)
          val step1: Int = x100.content()(1)
          val width1: Int = x92.content()(1)
          val ll1: Int = if (x25) lb1 + 1 else lb1
          val ul1: Int = if (x25) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x92.content()(2)
              val ub2: Int = x99.content()(2)
              val step2: Int = x100.content()(2)
              val width2: Int = x92.content()(2)
              val ll2: Int = if (x25) lb2 + 1 else lb2
              val ul2: Int = if (x25) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x101: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x101
                  val feval: MDArray[Double] = {
                    // RuntimeCheck : POST:   S87 = [u382  u381  u380]                               from Bubble up shape for Sym(87) <- Sel(Sym(101), Sym(87))
                    // RuntimeCheck : POST:   V101 = [u487(<u382)  u488(<u381)  u489(<u380)]         from Bubble up value for Sym(101) <- Sel(Sym(101), Sym(87))
                    // RuntimeCheck : POST:   S101 = [3]                                             from Bubble up shape for Sym(101) <- Sel(Sym(101), Sym(87))
                    // RuntimeCheck : PRE:    length(S101) = length([u11077])                        from Sel(Sym(101), Sym(87))
                    // RuntimeCheck : PRE:    S87(:length(V101)) < V101                              from Sel(Sym(101), Sym(87))
                    // Shape: V132=[u460] and S132=[]
                    
                    // Shape: V132=[u460] and S132=[]
                    val x132: Double = x87.content()(flatten(shape(x87), x101, "sel"))
                    // RuntimeCheck : POST:   V20 = [u12]                                            from Bubble up value for Sym(20) <- InfixOp(*: Sym(132) and Sym(20))
                    // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(*: Sym(132) and Sym(20))
                    // RuntimeCheck : POST:   V132 = [u460]                                          from Bubble up value for Sym(132) <- InfixOp(*: Sym(132) and Sym(20))
                    // RuntimeCheck : POST:   S132 = []                                              from Bubble up shape for Sym(132) <- InfixOp(*: Sym(132) and Sym(20))
                    // RuntimeCheck : PRE:    S132 = S20 OR S20 = []                                 from InfixOp(*: Sym(132) and Sym(20))
                    // Shape: V133=[u459] and S133=[]
                    val x133: Double = {
                      val result = new Array[Double](shape(x132).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x132.content()(i) *  x20
                      internalReshape(shape(x132), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V126 = [u262]                                          from Bubble up value for Sym(126) <- FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : POST:   S126 = []                                              from Bubble up shape for Sym(126) <- FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : POST:   S125 = []                                              from Bubble up shape for Sym(125) <- FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : POST:   S124 = []                                              from Bubble up shape for Sym(124) <- FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : POST:   V115 = [u13]                                           from Bubble up value for Sym(115) <- FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : POST:   S115 = []                                              from Bubble up shape for Sym(115) <- FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : POST:   V129 = [u467]                                          from Bubble up value for Sym(129) <- FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : POST:   S129 = []                                              from Bubble up shape for Sym(129) <- FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : PRE:    S115 = S129                                            from FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // RuntimeCheck : PRE:    S126 = S129                                            from FoldArrayWith(Sym(115), fold (Sym(124), Sym(125)) => Sym(126), Sym(129))
                    // Shape: V130=[u261] and S130=[]
                    
                    val x130: Double = {
                      val opName: String = "fold"
                      var result: MDArray[Double] = x115
                      val foldFunction: (MDArray[Double], MDArray[Double]) => MDArray[Double] = (x124, x125) => {
                        // RuntimeCheck : POST:   S125 = []                                              from Bubble up shape for Sym(125) <- ScalarOperator Sym(124) + Sym(125)
                        // RuntimeCheck : POST:   S124 = []                                              from Bubble up shape for Sym(124) <- ScalarOperator Sym(124) + Sym(125)
                        // RuntimeCheck : PRE:    S124 = []                                              from ScalarOperator Sym(124) + Sym(125)
                        // RuntimeCheck : PRE:    S125 = []                                              from ScalarOperator Sym(124) + Sym(125)
                        // Shape: V126=[u262] and S126=[]
                        val x126: Double = ((a: Double, b: Double) => a + b)(x124, x125)
                        x126
                      }
                      // Shape: V4=[u2] and S4=[]
                      val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
                      // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
                      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
                      // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
                      // Shape: V5=[u230] and S5=[]
                      val x5: Boolean = x4
                      // RuntimeCheck : POST:   V5 = [u230]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
                      // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
                      // Shape: V14=[u229] and S14=[]
                      val x14: Boolean = x5
                      // RuntimeCheck : POST:   V14 = [u229]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
                      // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
                      // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
                      // Shape: V15=[u228] and S15=[]
                      val x15: Boolean = x14
                      // Shape: V96=[3  3  3] and S96=[3]
                      val x96: MDArray[Int] = internalReshape(3::Nil, Array(3, 3, 3), "knownAtCompileTime")
                      // Shape: V97=[u16  u17  u18  u1 ... 39  u40  u41  u42] and S97=[27]
                      val x97: MDArray[Double] = internalReshape(27::Nil, Array(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), "knownAtCompileTime")
                      // RuntimeCheck : POST:   V97 = [u16  u17  u18  u19  u20  u21  u22  u23  u24  u25  u26  u27  u28  u29  u30  u31  u32  u33  u34  u35  u36  u37  u38  u39  u40  u41  u42]     from Bubble up value for Sym(97) <- Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : POST:   S97 = [27]                                             from Bubble up shape for Sym(97) <- Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : POST:   V96 = [3  3  3]                                        from Bubble up value for Sym(96) <- Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : POST:   S96 = [3]                                              from Bubble up shape for Sym(96) <- Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : PRE:    length(S96) = length([u11983])                         from Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : PRE:    prod(V96) = prod(S97)                                  from Reshape(Sym(96), Sym(97))
                      // Shape: V98=[u227  u226  u225  ...  u203  u202  u201] and S98=[3  3  3]
                      val x98: MDArray[Double] = reshape(x96, x97)
                      // RuntimeCheck : POST:   V98 = [u227  u226  u225  u224  u223  u222  u221  u220  u219  u218  u217  u216  u215  u214  u213  u212  u211  u210  u209  u208  u207  u206  u205  u204  u203  u202  u201]     from Bubble up value for Sym(98) <- Shape(Sym(98))
                      // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- Shape(Sym(98))
                      // Shape: V102=[3  3  3] and S102=[3]
                      val x102: MDArray[Int] = shape(x98)
                      // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- GenArrayWith(Sym(102) - Sym(112))
                      // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- GenArrayWith(Sym(102) - Sym(112))
                      // RuntimeCheck : POST:   V112 = [u422]                                          from Bubble up value for Sym(112) <- GenArrayWith(Sym(102) - Sym(112))
                      // RuntimeCheck : POST:   S112 = []                                              from Bubble up shape for Sym(112) <- GenArrayWith(Sym(102) - Sym(112))
                      // RuntimeCheck : PRE:    S102 = [u11855]                                        from GenArrayWith(Sym(102) - Sym(112))
                      // RuntimeCheck : PRE:    S102 = S106                                            from GenArrayWith(Sym(102) - Sym(112))
                      // RuntimeCheck : PRE:    V102(:length(V106)) < V106                             from GenArrayWith(Sym(102) - Sym(112))
                      // RuntimeCheck : PRE:    V102(length(V106):) = S112                             from GenArrayWith(Sym(102) - Sym(112))
                      // Shape: V113=[u421  u420  u419  ...  u397  u396  u395] and S113=[3  3  3]
                      
                      val x113: MDArray[Double] = {
                        val opName: String = "genarray"
                        var result: Array[Double] = null
                        var rshape: Array[Int] = null
                        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- Shape(Sym(102))
                        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- Shape(Sym(102))
                        // Shape: V104=[3] and S104=[1]
                        val x104: MDArray[Int] = shape(x102)
                        // RuntimeCheck : POST:   V104 = [3]                                             from Bubble up value for Sym(104) <- Sel(Sym(9), Sym(104))
                        // RuntimeCheck : POST:   S104 = [1]                                             from Bubble up shape for Sym(104) <- Sel(Sym(9), Sym(104))
                        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(104))
                        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(104))
                        // RuntimeCheck : PRE:    length(S9) = length([u11950])                          from Sel(Sym(9), Sym(104))
                        // RuntimeCheck : PRE:    S104(:length(V9)) < V9                                 from Sel(Sym(9), Sym(104))
                        // Shape: V105=[3] and S105=[]
                        
                        // Shape: V105=[3] and S105=[]
                        val x105: Int = x104.content()(flatten(shape(x104), x9, "sel"))
                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(105))
                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(105))
                        // RuntimeCheck : POST:   V105 = [3]                                             from Bubble up value for Sym(105) <- Values(Sym(12), Sym(105))
                        // RuntimeCheck : POST:   S105 = []                                              from Bubble up shape for Sym(105) <- Values(Sym(12), Sym(105))
                        // RuntimeCheck : PRE:    S105 = []                                              from Values(Sym(12), Sym(105))
                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(105))
                        // Shape: V106=[0  0  0] and S106=[3]
                        val x106: MDArray[Int] = {
                          val result = new Array[Int](x105)
                          for(i <- List.range(0, result.length))
                          result(i) = x12
                          internalReshape(x105::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
                        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
                        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
                        // RuntimeCheck : PRE:    S102 = S7 OR S7 = []                                   from InfixOp(-: Sym(102) and Sym(7))
                        // Shape: V107=[u304  u303  u302] and S107=[3]
                        val x107: MDArray[Int] = {
                          val result = new Array[Int](shape(x102).content().foldLeft(1)((a,b) => a*b))
                          for(i <- List.range(0, result.length))
                          result(i) = x102.content()(i) -  x7
                          internalReshape(shape(x102), result, "infixOpAA")
                        }
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(105))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(105))
                        // RuntimeCheck : POST:   V105 = [3]                                             from Bubble up value for Sym(105) <- Values(Sym(7), Sym(105))
                        // RuntimeCheck : POST:   S105 = []                                              from Bubble up shape for Sym(105) <- Values(Sym(7), Sym(105))
                        // RuntimeCheck : PRE:    S105 = []                                              from Values(Sym(7), Sym(105))
                        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(105))
                        // Shape: V108=[1  1  1] and S108=[3]
                        val x108: MDArray[Int] = {
                          val result = new Array[Int](x105)
                          for(i <- List.range(0, result.length))
                          result(i) = x7
                          internalReshape(x105::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(101) and Sym(7))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(101) and Sym(7))
                        // RuntimeCheck : POST:   V101 = [u487(<u382)  u488(<u381)  u489(<u380)]         from Bubble up value for Sym(101) <- InfixOp(-: Sym(101) and Sym(7))
                        // RuntimeCheck : POST:   S101 = [3]                                             from Bubble up shape for Sym(101) <- InfixOp(-: Sym(101) and Sym(7))
                        // RuntimeCheck : PRE:    S101 = S7 OR S7 = []                                   from InfixOp(-: Sym(101) and Sym(7))
                        // Shape: V103=[u449  u448  u447] and S103=[3]
                        val x103: MDArray[Int] = {
                          val result = new Array[Int](shape(x101).content().foldLeft(1)((a,b) => a*b))
                          for(i <- List.range(0, result.length))
                          result(i) = x101.content()(i) -  x7
                          internalReshape(shape(x101), result, "infixOpAA")
                        }
                        // RuntimeCheck : POST:   V111 = [u423]                                          from Bubble up value for Sym(111) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   S111 = []                                              from Bubble up shape for Sym(111) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   S109 = [3]                                             from Bubble up shape for Sym(109) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   V106 = [0  0  0]                                       from Bubble up value for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   S106 = [3]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   V108 = [1  1  1]                                       from Bubble up value for Sym(108) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   S108 = [3]                                             from Bubble up shape for Sym(108) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   V15 = [u228]                                           from Bubble up value for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   V107 = [u304  u303  u302]                              from Bubble up value for Sym(107) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   S107 = [3]                                             from Bubble up shape for Sym(107) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   V15 = [u228]                                           from Bubble up value for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   V106 = [0  0  0]                                       from Bubble up value for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : POST:   S106 = [3]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : PRE:    length(S106) = length([u11856])                        from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : PRE:    S107 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : PRE:    S108 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : PRE:    S106 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // RuntimeCheck : PRE:    V106 < V107                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        // Shape: V112=[u422] and S112=[]
                        // with: With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(107) step=Sym(108) width=Sym(106)  Sym(109) => Sym(111))
                        val lb0: Int = x106.content()(0)
                        val ub0: Int = x107.content()(0)
                        val step0: Int = x108.content()(0)
                        val width0: Int = x106.content()(0)
                        val ll0: Int = if (x15) lb0 + 1 else lb0
                        val ul0: Int = if (x15) ub0 else ub0 + 1
                        for (iv0 <- List.range(ll0, ul0)) {
                          if ((iv0 - lb0) % step0 <= width0) {
                            val lb1: Int = x106.content()(1)
                            val ub1: Int = x107.content()(1)
                            val step1: Int = x108.content()(1)
                            val width1: Int = x106.content()(1)
                            val ll1: Int = if (x15) lb1 + 1 else lb1
                            val ul1: Int = if (x15) ub1 else ub1 + 1
                            for (iv1 <- List.range(ll1, ul1)) {
                              if ((iv1 - lb1) % step1 <= width1) {
                                val lb2: Int = x106.content()(2)
                                val ub2: Int = x107.content()(2)
                                val step2: Int = x108.content()(2)
                                val width2: Int = x106.content()(2)
                                val ll2: Int = if (x15) lb2 + 1 else lb2
                                val ul2: Int = if (x15) ub2 else ub2 + 1
                                for (iv2 <- List.range(ll2, ul2)) {
                                  if ((iv2 - lb2) % step2 <= width2) {
                                    val x109: MDArray[Int] = iv0::iv1::iv2::Nil
                                    val iv: MDArray[Int] = x109
                                    val feval: MDArray[Double] = {
                                      // RuntimeCheck : POST:   V103 = [u449  u448  u447]                              from Bubble up value for Sym(103) <- InfixOp(+: Sym(109) and Sym(103))
                                      // RuntimeCheck : POST:   S103 = [3]                                             from Bubble up shape for Sym(103) <- InfixOp(+: Sym(109) and Sym(103))
                                      // RuntimeCheck : POST:   S109 = [3]                                             from Bubble up shape for Sym(109) <- InfixOp(+: Sym(109) and Sym(103))
                                      // RuntimeCheck : PRE:    S109 = S103 OR S103 = []                               from InfixOp(+: Sym(109) and Sym(103))
                                      // Shape: V110=[u484(<u382)  u485(<u381)  u486(<u380)] and S110=[3]
                                      val x110: MDArray[Int] = {
                                        val result = new Array[Int](shape(x109).content().foldLeft(1)((a,b) => a*b))
                                        for(i <- List.range(0, result.length))
                                        result(i) = x109.content()(i) +  x103.content()(i)
                                        internalReshape(shape(x109), result, "infixOpAA")
                                      }
                                      // RuntimeCheck : POST:   S87 = [u382  u381  u380]                               from Bubble up shape for Sym(87) <- Sel(Sym(110), Sym(87))
                                      // RuntimeCheck : POST:   V110 = [u484(<u382)  u485(<u381)  u486(<u380)]         from Bubble up value for Sym(110) <- Sel(Sym(110), Sym(87))
                                      // RuntimeCheck : POST:   S110 = [3]                                             from Bubble up shape for Sym(110) <- Sel(Sym(110), Sym(87))
                                      // RuntimeCheck : PRE:    length(S110) = length([u11981])                        from Sel(Sym(110), Sym(87))
                                      // RuntimeCheck : PRE:    S87(:length(V110)) < V110                              from Sel(Sym(110), Sym(87))
                                      // Shape: V111=[u423] and S111=[]
                                      
                                      // Shape: V111=[u423] and S111=[]
                                      val x111: Double = x87.content()(flatten(shape(x87), x110, "sel"))
                                      x111
                                    }
                                    // the action of this loop:
                                    if (result == null) {
                                      // create the array and shape
                                      result = new Array[Double](x102.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                      rshape = shape(feval).content()
                                    } else {
                                      // check shape -- this WILL be redundant due to runtime checks
                                      if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                                    }
                                    // copy new content
                                    val mainIndex: Int = flatten(x102 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                                    for (innerIndex <- List.range(0, rshape.length)) {
                                      result(mainIndex + innerIndex) = feval(innerIndex)
                                    }
                                  } // if ((iv0 ...
                                } // for (iv0 ...
                              } // if ((iv1 ...
                            } // for (iv1 ...
                          } // if ((iv2 ...
                        } // for (iv2 ...
                        internalReshape(x102 ::: rshape.toList, result, opName)
                      }
                      
                      // RuntimeCheck : POST:   V113 = [u421  u420  u419  u418  u417  u416  u415  u414  u413  u412  u411  u410  u409  u408  u407  u406  u405  u404  u403  u402  u401  u400  u399  u398  u397  u396  u395]     from Bubble up value for Sym(113) <- InfixOp(*: Sym(98) and Sym(113))
                      // RuntimeCheck : POST:   S113 = [3  3  3]                                       from Bubble up shape for Sym(113) <- InfixOp(*: Sym(98) and Sym(113))
                      // RuntimeCheck : POST:   V98 = [u227  u226  u225  u224  u223  u222  u221  u220  u219  u218  u217  u216  u215  u214  u213  u212  u211  u210  u209  u208  u207  u206  u205  u204  u203  u202  u201]     from Bubble up value for Sym(98) <- InfixOp(*: Sym(98) and Sym(113))
                      // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- InfixOp(*: Sym(98) and Sym(113))
                      // RuntimeCheck : PRE:    S98 = S113 OR S113 = []                                from InfixOp(*: Sym(98) and Sym(113))
                      // Shape: V114=[u299  u298  u297  ...  u275  u274  u273] and S114=[3  3  3]
                      val x114: MDArray[Double] = {
                        val result = new Array[Double](shape(x98).content().foldLeft(1)((a,b) => a*b))
                        for(i <- List.range(0, result.length))
                        result(i) = x98.content()(i) *  x113.content()(i)
                        internalReshape(shape(x98), result, "infixOpAA")
                      }
                      // RuntimeCheck : POST:   V114 = [u299  u298  u297  u296  u295  u294  u293  u292  u291  u290  u289  u288  u287  u286  u285  u284  u283  u282  u281  u280  u279  u278  u277  u276  u275  u274  u273]     from Bubble up value for Sym(114) <- Dim(Sym(114))
                      // RuntimeCheck : POST:   S114 = [3  3  3]                                       from Bubble up shape for Sym(114) <- Dim(Sym(114))
                      // Shape: V116=[3] and S116=[]
                      val x116: Int = dim(x114)
                      // RuntimeCheck : POST:   V116 = [3]                                             from Bubble up value for Sym(116) <- FromValue(Sym(116))
                      // RuntimeCheck : POST:   S116 = []                                              from Bubble up shape for Sym(116) <- FromValue(Sym(116))
                      // Shape: V117=[3] and S117=[]
                      val x117: Int = x116
                      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(117))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(117))
                      // RuntimeCheck : POST:   V117 = [3]                                             from Bubble up value for Sym(117) <- Values(Sym(12), Sym(117))
                      // RuntimeCheck : POST:   S117 = []                                              from Bubble up shape for Sym(117) <- Values(Sym(12), Sym(117))
                      // RuntimeCheck : PRE:    S117 = []                                              from Values(Sym(12), Sym(117))
                      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(117))
                      // Shape: V118=[0  0  0] and S118=[3]
                      val x118: MDArray[Int] = {
                        val result = new Array[Int](x117)
                        for(i <- List.range(0, result.length))
                        result(i) = x12
                        internalReshape(x117::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V114 = [u299  u298  u297  u296  u295  u294  u293  u292  u291  u290  u289  u288  u287  u286  u285  u284  u283  u282  u281  u280  u279  u278  u277  u276  u275  u274  u273]     from Bubble up value for Sym(114) <- Shape(Sym(114))
                      // RuntimeCheck : POST:   S114 = [3  3  3]                                       from Bubble up shape for Sym(114) <- Shape(Sym(114))
                      // Shape: V119=[3  3  3] and S119=[3]
                      val x119: MDArray[Int] = shape(x114)
                      // RuntimeCheck : POST:   V118 = [0  0  0]                                       from Bubble up value for Sym(118) <- Shape(Sym(118))
                      // RuntimeCheck : POST:   S118 = [3]                                             from Bubble up shape for Sym(118) <- Shape(Sym(118))
                      // Shape: V120=[3] and S120=[1]
                      val x120: MDArray[Int] = shape(x118)
                      // RuntimeCheck : POST:   V120 = [3]                                             from Bubble up value for Sym(120) <- Sel(Sym(9), Sym(120))
                      // RuntimeCheck : POST:   S120 = [1]                                             from Bubble up shape for Sym(120) <- Sel(Sym(9), Sym(120))
                      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(120))
                      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(120))
                      // RuntimeCheck : PRE:    length(S9) = length([u11639])                          from Sel(Sym(9), Sym(120))
                      // RuntimeCheck : PRE:    S120(:length(V9)) < V9                                 from Sel(Sym(9), Sym(120))
                      // Shape: V121=[3] and S121=[]
                      
                      // Shape: V121=[3] and S121=[]
                      val x121: Int = x120.content()(flatten(shape(x120), x9, "sel"))
                      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(121))
                      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(121))
                      // RuntimeCheck : POST:   V121 = [3]                                             from Bubble up value for Sym(121) <- Values(Sym(7), Sym(121))
                      // RuntimeCheck : POST:   S121 = []                                              from Bubble up shape for Sym(121) <- Values(Sym(7), Sym(121))
                      // RuntimeCheck : PRE:    S121 = []                                              from Values(Sym(7), Sym(121))
                      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(121))
                      // Shape: V122=[1  1  1] and S122=[3]
                      val x122: MDArray[Int] = {
                        val result = new Array[Int](x121)
                        for(i <- List.range(0, result.length))
                        result(i) = x7
                        internalReshape(x121::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(121))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(121))
                      // RuntimeCheck : POST:   V121 = [3]                                             from Bubble up value for Sym(121) <- Values(Sym(12), Sym(121))
                      // RuntimeCheck : POST:   S121 = []                                              from Bubble up shape for Sym(121) <- Values(Sym(12), Sym(121))
                      // RuntimeCheck : PRE:    S121 = []                                              from Values(Sym(12), Sym(121))
                      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(121))
                      // Shape: V123=[0  0  0] and S123=[3]
                      val x123: MDArray[Int] = {
                        val result = new Array[Int](x121)
                        for(i <- List.range(0, result.length))
                        result(i) = x12
                        internalReshape(x121::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V128 = [u468]                                          from Bubble up value for Sym(128) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   S128 = []                                              from Bubble up shape for Sym(128) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   V127 = [u481(<3)  u482(<3)  u483(<3)]                  from Bubble up value for Sym(127) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   S127 = [3]                                             from Bubble up shape for Sym(127) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   V123 = [0  0  0]                                       from Bubble up value for Sym(123) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   S123 = [3]                                             from Bubble up shape for Sym(123) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   V122 = [1  1  1]                                       from Bubble up value for Sym(122) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   S122 = [3]                                             from Bubble up shape for Sym(122) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   V25 = [u248]                                           from Bubble up value for Sym(25) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   V119 = [3  3  3]                                       from Bubble up value for Sym(119) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   S119 = [3]                                             from Bubble up shape for Sym(119) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   V15 = [u228]                                           from Bubble up value for Sym(15) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   V118 = [0  0  0]                                       from Bubble up value for Sym(118) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : POST:   S118 = [3]                                             from Bubble up shape for Sym(118) <- With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : PRE:    length(S118) = length([u11079])                        from With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : PRE:    S119 = S118                                            from With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : PRE:    S122 = S118                                            from With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : PRE:    S123 = S118                                            from With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // RuntimeCheck : PRE:    V118 < V119                                            from With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      // Shape: V129=[u467] and S129=[]
                      // with: With(lb=Sym(118) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(119) step=Sym(122) width=Sym(123)  Sym(127) => Sym(128))
                      val lb0: Int = x118.content()(0)
                      val ub0: Int = x119.content()(0)
                      val step0: Int = x122.content()(0)
                      val width0: Int = x123.content()(0)
                      val ll0: Int = if (x15) lb0 + 1 else lb0
                      val ul0: Int = if (x25) ub0 else ub0 + 1
                      for (iv0 <- List.range(ll0, ul0)) {
                        if ((iv0 - lb0) % step0 <= width0) {
                          val lb1: Int = x118.content()(1)
                          val ub1: Int = x119.content()(1)
                          val step1: Int = x122.content()(1)
                          val width1: Int = x123.content()(1)
                          val ll1: Int = if (x15) lb1 + 1 else lb1
                          val ul1: Int = if (x25) ub1 else ub1 + 1
                          for (iv1 <- List.range(ll1, ul1)) {
                            if ((iv1 - lb1) % step1 <= width1) {
                              val lb2: Int = x118.content()(2)
                              val ub2: Int = x119.content()(2)
                              val step2: Int = x122.content()(2)
                              val width2: Int = x123.content()(2)
                              val ll2: Int = if (x15) lb2 + 1 else lb2
                              val ul2: Int = if (x25) ub2 else ub2 + 1
                              for (iv2 <- List.range(ll2, ul2)) {
                                if ((iv2 - lb2) % step2 <= width2) {
                                  val x127: MDArray[Int] = iv0::iv1::iv2::Nil
                                  val iv: MDArray[Int] = x127
                                  val feval: MDArray[Double] = {
                                    // RuntimeCheck : POST:   V114 = [u299  u298  u297  u296  u295  u294  u293  u292  u291  u290  u289  u288  u287  u286  u285  u284  u283  u282  u281  u280  u279  u278  u277  u276  u275  u274  u273]     from Bubble up value for Sym(114) <- Sel(Sym(127), Sym(114))
                                    // RuntimeCheck : POST:   S114 = [3  3  3]                                       from Bubble up shape for Sym(114) <- Sel(Sym(127), Sym(114))
                                    // RuntimeCheck : POST:   V127 = [u481(<3)  u482(<3)  u483(<3)]                  from Bubble up value for Sym(127) <- Sel(Sym(127), Sym(114))
                                    // RuntimeCheck : POST:   S127 = [3]                                             from Bubble up shape for Sym(127) <- Sel(Sym(127), Sym(114))
                                    // RuntimeCheck : PRE:    length(S127) = length([u11826])                        from Sel(Sym(127), Sym(114))
                                    // RuntimeCheck : PRE:    S114(:length(V127)) < V127                             from Sel(Sym(127), Sym(114))
                                    // Shape: V128=[u468] and S128=[]
                                    
                                    // Shape: V128=[u468] and S128=[]
                                    val x128: Double = x114.content()(flatten(shape(x114), x127, "sel"))
                                    x128
                                  }
                                  // the action of this loop:
                                  result = foldFunction(result, feval)
                                } // if ((iv0 ...
                              } // for (iv0 ...
                            } // if ((iv1 ...
                          } // for (iv1 ...
                        } // if ((iv2 ...
                      } // for (iv2 ...
                      result
                    }
                    
                    // RuntimeCheck : POST:   V130 = [u261]                                          from Bubble up value for Sym(130) <- ToValue(Sym(130))
                    // RuntimeCheck : POST:   S130 = []                                              from Bubble up shape for Sym(130) <- ToValue(Sym(130))
                    // RuntimeCheck : PRE:    length(S130) = length([])                              from ToValue(Sym(130))
                    // Shape: V131=[u260] and S131=[]
                    val x131: Double = x130
                    // RuntimeCheck : POST:   V131 = [u260]                                          from Bubble up value for Sym(131) <- FromValue(Sym(131))
                    // RuntimeCheck : POST:   S131 = []                                              from Bubble up shape for Sym(131) <- FromValue(Sym(131))
                    // Shape: V134=[u259] and S134=[]
                    val x134: Double = x131
                    // RuntimeCheck : POST:   V134 = [u259]                                          from Bubble up value for Sym(134) <- InfixOp(+: Sym(133) and Sym(134))
                    // RuntimeCheck : POST:   S134 = []                                              from Bubble up shape for Sym(134) <- InfixOp(+: Sym(133) and Sym(134))
                    // RuntimeCheck : POST:   V133 = [u459]                                          from Bubble up value for Sym(133) <- InfixOp(+: Sym(133) and Sym(134))
                    // RuntimeCheck : POST:   S133 = []                                              from Bubble up shape for Sym(133) <- InfixOp(+: Sym(133) and Sym(134))
                    // RuntimeCheck : PRE:    S133 = S134 OR S134 = []                               from InfixOp(+: Sym(133) and Sym(134))
                    // Shape: V135=[u458] and S135=[]
                    val x135: Double = {
                      val result = new Array[Double](shape(x133).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x133.content()(i) +  x134
                      internalReshape(shape(x133), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V49 = [u11]                                            from Bubble up value for Sym(49) <- InfixOp(*: Sym(135) and Sym(49))
                    // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- InfixOp(*: Sym(135) and Sym(49))
                    // RuntimeCheck : POST:   V135 = [u458]                                          from Bubble up value for Sym(135) <- InfixOp(*: Sym(135) and Sym(49))
                    // RuntimeCheck : POST:   S135 = []                                              from Bubble up shape for Sym(135) <- InfixOp(*: Sym(135) and Sym(49))
                    // RuntimeCheck : PRE:    S135 = S49 OR S49 = []                                 from InfixOp(*: Sym(135) and Sym(49))
                    // Shape: V136=[u457] and S136=[]
                    val x136: Double = {
                      val result = new Array[Double](shape(x135).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x135.content()(i) *  x49
                      internalReshape(shape(x135), result, "infixOpAA")
                    }
                    x136
                  }
                  // the action of this loop:
                  if (rshape == null) {
                    rshape = shape(x87).drop(iv.content().length)
                  }
                  val mainIndex: Int = flatten(shape(x87), iv ::: zeros(dim(x87) - iv.content().length), opName)
                  // check shape -- this WILL be redundant due to runtime checks
                  if (shape(feval).content().toList != rshape) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                  // copy new content
                  for (innerIndex <- List.range(0, feval.content().length)) {
                    result(mainIndex + innerIndex) = feval.content()(innerIndex)
                  }
                } // if ((iv0 ...
              } // for (iv0 ...
            } // if ((iv1 ...
          } // for (iv1 ...
        } // if ((iv2 ...
      } // for (iv2 ...
      internalReshape(shape(x87) ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S87 = [u382  u381  u380]                               from Bubble up shape for Sym(87) <- Where(Sym(95), Sym(138), Sym(87))
    // RuntimeCheck : POST:   S138 = [u382  u381  u380]                              from Bubble up shape for Sym(138) <- Where(Sym(95), Sym(138), Sym(87))
    // RuntimeCheck : POST:   S95 = [u382  u381  u380]                               from Bubble up shape for Sym(95) <- Where(Sym(95), Sym(138), Sym(87))
    // RuntimeCheck : PRE:    S95 = S138                                             from Where(Sym(95), Sym(138), Sym(87))
    // RuntimeCheck : PRE:    S95 = S87                                              from Where(Sym(95), Sym(138), Sym(87))
    // Shape: S139=[u382  u381  u380]
    val x139: MDArray[Double] = {
      val result = new Array[Double](shape(x138).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = if (x95.content()(i)) x138.content()(i) else x87.content()(i)
      internalReshape(shape(x138), result, "where")
    }
    // RuntimeCheck : POST:   S95 = [u382  u381  u380]                               from Bubble up shape for Sym(95) <- UnaryOp(!: Sym(95))
    // Shape: S140=[u382  u381  u380]
    val x140: MDArray[Boolean] = {
      val result = new Array[Boolean](shape(x95).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = !x95.content()(i)
      internalReshape(shape(x95), result, "unaryOp")
    }
    // RuntimeCheck : POST:   S139 = [u382  u381  u380]                              from Bubble up shape for Sym(139) <- ModArrayWith(Sym(139) - Sym(177))
    // RuntimeCheck : POST:   V177 = [u430]                                          from Bubble up value for Sym(177) <- ModArrayWith(Sym(139) - Sym(177))
    // RuntimeCheck : POST:   S177 = []                                              from Bubble up shape for Sym(177) <- ModArrayWith(Sym(139) - Sym(177))
    // RuntimeCheck : PRE:    S144 = [u503]                                          from ModArrayWith(Sym(139) - Sym(177))
    // RuntimeCheck : PRE:    S144 = [LengthOf(S139)]                                from ModArrayWith(Sym(139) - Sym(177))
    // RuntimeCheck : PRE:    S139(:length(V144)) < V144                             from ModArrayWith(Sym(139) - Sym(177))
    // RuntimeCheck : PRE:    S139(length(V144):) = S177                             from ModArrayWith(Sym(139) - Sym(177))
    // Shape: S178=[u382  u381  u380]
    
    val x178: MDArray[Double] = {
      val opName: String = "modarray"
      var result: Array[Double] = new Array[Double](shape(x139).content().foldLeft(1)((a,b) => a*b))
      for (i <- List.range(0, result.length)) {
        result(i) = x139.content()(i)
      }
      var rshape: List[Int] = null
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : PRE:    length(S17) = length([])                               from ToValue(Sym(17))
      // Shape: V21=[u250] and S21=[]
      val x21: Boolean = x17
      // RuntimeCheck : POST:   V21 = [u250]                                           from Bubble up value for Sym(21) <- FromValue(Sym(21))
      // RuntimeCheck : POST:   S21 = []                                               from Bubble up shape for Sym(21) <- FromValue(Sym(21))
      // Shape: V24=[u249] and S24=[]
      val x24: Boolean = x21
      // RuntimeCheck : POST:   V24 = [u249]                                           from Bubble up value for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : PRE:    length(S24) = length([])                               from ToValue(Sym(24))
      // Shape: V25=[u248] and S25=[]
      val x25: Boolean = x24
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   S139 = [u382  u381  u380]                              from Bubble up shape for Sym(139) <- Shape(Sym(139))
      // Shape: V141=[u382  u381  u380] and S141=[3]
      val x141: MDArray[Int] = shape(x139)
      // RuntimeCheck : POST:   V141 = [u382  u381  u380]                              from Bubble up value for Sym(141) <- Shape(Sym(141))
      // RuntimeCheck : POST:   S141 = [3]                                             from Bubble up shape for Sym(141) <- Shape(Sym(141))
      // Shape: V142=[3] and S142=[1]
      val x142: MDArray[Int] = shape(x141)
      // RuntimeCheck : POST:   V142 = [3]                                             from Bubble up value for Sym(142) <- Sel(Sym(9), Sym(142))
      // RuntimeCheck : POST:   S142 = [1]                                             from Bubble up shape for Sym(142) <- Sel(Sym(9), Sym(142))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(142))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(142))
      // RuntimeCheck : PRE:    length(S9) = length([u3394])                           from Sel(Sym(9), Sym(142))
      // RuntimeCheck : PRE:    S142(:length(V9)) < V9                                 from Sel(Sym(9), Sym(142))
      // Shape: V143=[3] and S143=[]
      
      // Shape: V143=[3] and S143=[]
      val x143: Int = x142.content()(flatten(shape(x142), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(143))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(143))
      // RuntimeCheck : POST:   V143 = [3]                                             from Bubble up value for Sym(143) <- Values(Sym(12), Sym(143))
      // RuntimeCheck : POST:   S143 = []                                              from Bubble up shape for Sym(143) <- Values(Sym(12), Sym(143))
      // RuntimeCheck : PRE:    S143 = []                                              from Values(Sym(12), Sym(143))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(143))
      // Shape: V144=[0  0  0] and S144=[3]
      val x144: MDArray[Int] = {
        val result = new Array[Int](x143)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x143::Nil, result, "values")
      }
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(141) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(141) and Sym(7))
      // RuntimeCheck : POST:   V141 = [u382  u381  u380]                              from Bubble up value for Sym(141) <- InfixOp(-: Sym(141) and Sym(7))
      // RuntimeCheck : POST:   S141 = [3]                                             from Bubble up shape for Sym(141) <- InfixOp(-: Sym(141) and Sym(7))
      // RuntimeCheck : PRE:    S141 = S7 OR S7 = []                                   from InfixOp(-: Sym(141) and Sym(7))
      // Shape: V145=[u391  u390  u389] and S145=[3]
      val x145: MDArray[Int] = {
        val result = new Array[Int](shape(x141).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x141.content()(i) -  x7
        internalReshape(shape(x141), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(143))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(143))
      // RuntimeCheck : POST:   V143 = [3]                                             from Bubble up value for Sym(143) <- Values(Sym(7), Sym(143))
      // RuntimeCheck : POST:   S143 = []                                              from Bubble up shape for Sym(143) <- Values(Sym(7), Sym(143))
      // RuntimeCheck : PRE:    S143 = []                                              from Values(Sym(7), Sym(143))
      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(143))
      // Shape: V146=[1  1  1] and S146=[3]
      val x146: MDArray[Int] = {
        val result = new Array[Int](x143)
        for(i <- List.range(0, result.length))
        result(i) = x7
        internalReshape(x143::Nil, result, "values")
      }
      // Shape: V49=[u11] and S49=[]
      val x49: Double = internalReshape(Nil, Array(0.16666666666666666), "knownAtCompileTime")
      // Shape: V20=[u12] and S20=[]
      val x20: Double = internalReshape(Nil, Array(0.1), "knownAtCompileTime")
      // Shape: V115=[u13] and S115=[]
      val x115: Double = internalReshape(Nil, Array(0.0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V176 = [u431]                                          from Bubble up value for Sym(176) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   S176 = []                                              from Bubble up shape for Sym(176) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   V147 = [u444(<u382)  u445(<u381)  u446(<u380)]         from Bubble up value for Sym(147) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   S147 = [3]                                             from Bubble up shape for Sym(147) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   V144 = [0  0  0]                                       from Bubble up value for Sym(144) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   S144 = [3]                                             from Bubble up shape for Sym(144) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   V146 = [1  1  1]                                       from Bubble up value for Sym(146) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   S146 = [3]                                             from Bubble up shape for Sym(146) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   V25 = [u248]                                           from Bubble up value for Sym(25) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   V145 = [u391  u390  u389]                              from Bubble up value for Sym(145) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   S145 = [3]                                             from Bubble up shape for Sym(145) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   V25 = [u248]                                           from Bubble up value for Sym(25) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   V144 = [0  0  0]                                       from Bubble up value for Sym(144) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : POST:   S144 = [3]                                             from Bubble up shape for Sym(144) <- With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : PRE:    length(S144) = length([u504])                          from With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : PRE:    S145 = S144                                            from With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : PRE:    S146 = S144                                            from With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : PRE:    S144 = S144                                            from With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // RuntimeCheck : PRE:    V144 < V145                                            from With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      // Shape: V177=[u430] and S177=[]
      // with: With(lb=Sym(144) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(145) step=Sym(146) width=Sym(144)  Sym(147) => Sym(176))
      val lb0: Int = x144.content()(0)
      val ub0: Int = x145.content()(0)
      val step0: Int = x146.content()(0)
      val width0: Int = x144.content()(0)
      val ll0: Int = if (x25) lb0 + 1 else lb0
      val ul0: Int = if (x25) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x144.content()(1)
          val ub1: Int = x145.content()(1)
          val step1: Int = x146.content()(1)
          val width1: Int = x144.content()(1)
          val ll1: Int = if (x25) lb1 + 1 else lb1
          val ul1: Int = if (x25) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x144.content()(2)
              val ub2: Int = x145.content()(2)
              val step2: Int = x146.content()(2)
              val width2: Int = x144.content()(2)
              val ll2: Int = if (x25) lb2 + 1 else lb2
              val ul2: Int = if (x25) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x147: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x147
                  val feval: MDArray[Double] = {
                    // RuntimeCheck : POST:   S87 = [u382  u381  u380]                               from Bubble up shape for Sym(87) <- Sel(Sym(147), Sym(87))
                    // RuntimeCheck : POST:   V147 = [u444(<u382)  u445(<u381)  u446(<u380)]         from Bubble up value for Sym(147) <- Sel(Sym(147), Sym(87))
                    // RuntimeCheck : POST:   S147 = [3]                                             from Bubble up shape for Sym(147) <- Sel(Sym(147), Sym(87))
                    // RuntimeCheck : PRE:    length(S147) = length([u4357])                         from Sel(Sym(147), Sym(87))
                    // RuntimeCheck : PRE:    S87(:length(V147)) < V147                              from Sel(Sym(147), Sym(87))
                    // Shape: V172=[u434] and S172=[]
                    
                    // Shape: V172=[u434] and S172=[]
                    val x172: Double = x87.content()(flatten(shape(x87), x147, "sel"))
                    // RuntimeCheck : POST:   V20 = [u12]                                            from Bubble up value for Sym(20) <- InfixOp(*: Sym(172) and Sym(20))
                    // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(*: Sym(172) and Sym(20))
                    // RuntimeCheck : POST:   V172 = [u434]                                          from Bubble up value for Sym(172) <- InfixOp(*: Sym(172) and Sym(20))
                    // RuntimeCheck : POST:   S172 = []                                              from Bubble up shape for Sym(172) <- InfixOp(*: Sym(172) and Sym(20))
                    // RuntimeCheck : PRE:    S172 = S20 OR S20 = []                                 from InfixOp(*: Sym(172) and Sym(20))
                    // Shape: V173=[u433] and S173=[]
                    val x173: Double = {
                      val result = new Array[Double](shape(x172).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x172.content()(i) *  x20
                      internalReshape(shape(x172), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V166 = [u120]                                          from Bubble up value for Sym(166) <- FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : POST:   S166 = []                                              from Bubble up shape for Sym(166) <- FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : POST:   S165 = []                                              from Bubble up shape for Sym(165) <- FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : POST:   S164 = []                                              from Bubble up shape for Sym(164) <- FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : POST:   V115 = [u13]                                           from Bubble up value for Sym(115) <- FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : POST:   S115 = []                                              from Bubble up shape for Sym(115) <- FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : POST:   V169 = [u322]                                          from Bubble up value for Sym(169) <- FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : POST:   S169 = []                                              from Bubble up shape for Sym(169) <- FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : PRE:    S115 = S169                                            from FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // RuntimeCheck : PRE:    S166 = S169                                            from FoldArrayWith(Sym(115), fold (Sym(164), Sym(165)) => Sym(166), Sym(169))
                    // Shape: V170=[u233] and S170=[]
                    
                    val x170: Double = {
                      val opName: String = "fold"
                      var result: MDArray[Double] = x115
                      val foldFunction: (MDArray[Double], MDArray[Double]) => MDArray[Double] = (x164, x165) => {
                        // RuntimeCheck : POST:   S165 = []                                              from Bubble up shape for Sym(165) <- ScalarOperator Sym(164) + Sym(165)
                        // RuntimeCheck : POST:   S164 = []                                              from Bubble up shape for Sym(164) <- ScalarOperator Sym(164) + Sym(165)
                        // RuntimeCheck : PRE:    S164 = []                                              from ScalarOperator Sym(164) + Sym(165)
                        // RuntimeCheck : PRE:    S165 = []                                              from ScalarOperator Sym(164) + Sym(165)
                        // Shape: V166=[u120] and S166=[]
                        val x166: Double = ((a: Double, b: Double) => a + b)(x164, x165)
                        x166
                      }
                      // Shape: V4=[u2] and S4=[]
                      val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
                      // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
                      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
                      // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
                      // Shape: V5=[u230] and S5=[]
                      val x5: Boolean = x4
                      // RuntimeCheck : POST:   V5 = [u230]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
                      // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
                      // Shape: V14=[u229] and S14=[]
                      val x14: Boolean = x5
                      // RuntimeCheck : POST:   V14 = [u229]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
                      // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
                      // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
                      // Shape: V15=[u228] and S15=[]
                      val x15: Boolean = x14
                      // Shape: V96=[3  3  3] and S96=[3]
                      val x96: MDArray[Int] = internalReshape(3::Nil, Array(3, 3, 3), "knownAtCompileTime")
                      // Shape: V97=[u16  u17  u18  u1 ... 39  u40  u41  u42] and S97=[27]
                      val x97: MDArray[Double] = internalReshape(27::Nil, Array(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), "knownAtCompileTime")
                      // RuntimeCheck : POST:   V97 = [u16  u17  u18  u19  u20  u21  u22  u23  u24  u25  u26  u27  u28  u29  u30  u31  u32  u33  u34  u35  u36  u37  u38  u39  u40  u41  u42]     from Bubble up value for Sym(97) <- Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : POST:   S97 = [27]                                             from Bubble up shape for Sym(97) <- Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : POST:   V96 = [3  3  3]                                        from Bubble up value for Sym(96) <- Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : POST:   S96 = [3]                                              from Bubble up shape for Sym(96) <- Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : PRE:    length(S96) = length([u11983])                         from Reshape(Sym(96), Sym(97))
                      // RuntimeCheck : PRE:    prod(V96) = prod(S97)                                  from Reshape(Sym(96), Sym(97))
                      // Shape: V98=[u227  u226  u225  ...  u203  u202  u201] and S98=[3  3  3]
                      val x98: MDArray[Double] = reshape(x96, x97)
                      // RuntimeCheck : POST:   V98 = [u227  u226  u225  u224  u223  u222  u221  u220  u219  u218  u217  u216  u215  u214  u213  u212  u211  u210  u209  u208  u207  u206  u205  u204  u203  u202  u201]     from Bubble up value for Sym(98) <- Shape(Sym(98))
                      // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- Shape(Sym(98))
                      // Shape: V102=[3  3  3] and S102=[3]
                      val x102: MDArray[Int] = shape(x98)
                      // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- GenArrayWith(Sym(102) - Sym(153))
                      // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- GenArrayWith(Sym(102) - Sym(153))
                      // RuntimeCheck : POST:   V153 = [u378]                                          from Bubble up value for Sym(153) <- GenArrayWith(Sym(102) - Sym(153))
                      // RuntimeCheck : POST:   S153 = []                                              from Bubble up shape for Sym(153) <- GenArrayWith(Sym(102) - Sym(153))
                      // RuntimeCheck : PRE:    S102 = [u8975]                                         from GenArrayWith(Sym(102) - Sym(153))
                      // RuntimeCheck : PRE:    S102 = S106                                            from GenArrayWith(Sym(102) - Sym(153))
                      // RuntimeCheck : PRE:    V102(:length(V106)) < V106                             from GenArrayWith(Sym(102) - Sym(153))
                      // RuntimeCheck : PRE:    V102(length(V106):) = S153                             from GenArrayWith(Sym(102) - Sym(153))
                      // Shape: V154=[u377  u376  u375  ...  u353  u352  u351] and S154=[3  3  3]
                      
                      val x154: MDArray[Double] = {
                        val opName: String = "genarray"
                        var result: Array[Double] = null
                        var rshape: Array[Int] = null
                        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- Shape(Sym(102))
                        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- Shape(Sym(102))
                        // Shape: V104=[3] and S104=[1]
                        val x104: MDArray[Int] = shape(x102)
                        // RuntimeCheck : POST:   V104 = [3]                                             from Bubble up value for Sym(104) <- Sel(Sym(9), Sym(104))
                        // RuntimeCheck : POST:   S104 = [1]                                             from Bubble up shape for Sym(104) <- Sel(Sym(9), Sym(104))
                        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(104))
                        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(104))
                        // RuntimeCheck : PRE:    length(S9) = length([u11950])                          from Sel(Sym(9), Sym(104))
                        // RuntimeCheck : PRE:    S104(:length(V9)) < V9                                 from Sel(Sym(9), Sym(104))
                        // Shape: V105=[3] and S105=[]
                        
                        // Shape: V105=[3] and S105=[]
                        val x105: Int = x104.content()(flatten(shape(x104), x9, "sel"))
                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(105))
                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(105))
                        // RuntimeCheck : POST:   V105 = [3]                                             from Bubble up value for Sym(105) <- Values(Sym(12), Sym(105))
                        // RuntimeCheck : POST:   S105 = []                                              from Bubble up shape for Sym(105) <- Values(Sym(12), Sym(105))
                        // RuntimeCheck : PRE:    S105 = []                                              from Values(Sym(12), Sym(105))
                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(105))
                        // Shape: V106=[0  0  0] and S106=[3]
                        val x106: MDArray[Int] = {
                          val result = new Array[Int](x105)
                          for(i <- List.range(0, result.length))
                          result(i) = x12
                          internalReshape(x105::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(105))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(105))
                        // RuntimeCheck : POST:   V105 = [3]                                             from Bubble up value for Sym(105) <- Values(Sym(7), Sym(105))
                        // RuntimeCheck : POST:   S105 = []                                              from Bubble up shape for Sym(105) <- Values(Sym(7), Sym(105))
                        // RuntimeCheck : PRE:    S105 = []                                              from Values(Sym(7), Sym(105))
                        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(105))
                        // Shape: V108=[1  1  1] and S108=[3]
                        val x108: MDArray[Int] = {
                          val result = new Array[Int](x105)
                          for(i <- List.range(0, result.length))
                          result(i) = x7
                          internalReshape(x105::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
                        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
                        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
                        // RuntimeCheck : PRE:    S102 = S7 OR S7 = []                                   from InfixOp(-: Sym(102) and Sym(7))
                        // Shape: V149=[u173  u172  u171] and S149=[3]
                        val x149: MDArray[Int] = {
                          val result = new Array[Int](shape(x102).content().foldLeft(1)((a,b) => a*b))
                          for(i <- List.range(0, result.length))
                          result(i) = x102.content()(i) -  x7
                          internalReshape(shape(x102), result, "infixOpAA")
                        }
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(147) and Sym(7))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(147) and Sym(7))
                        // RuntimeCheck : POST:   V147 = [u444(<u382)  u445(<u381)  u446(<u380)]         from Bubble up value for Sym(147) <- InfixOp(-: Sym(147) and Sym(7))
                        // RuntimeCheck : POST:   S147 = [3]                                             from Bubble up shape for Sym(147) <- InfixOp(-: Sym(147) and Sym(7))
                        // RuntimeCheck : PRE:    S147 = S7 OR S7 = []                                   from InfixOp(-: Sym(147) and Sym(7))
                        // Shape: V148=[u385  u384  u383] and S148=[3]
                        val x148: MDArray[Int] = {
                          val result = new Array[Int](shape(x147).content().foldLeft(1)((a,b) => a*b))
                          for(i <- List.range(0, result.length))
                          result(i) = x147.content()(i) -  x7
                          internalReshape(shape(x147), result, "infixOpAA")
                        }
                        // RuntimeCheck : POST:   V152 = [u379]                                          from Bubble up value for Sym(152) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   S152 = []                                              from Bubble up shape for Sym(152) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   S150 = [3]                                             from Bubble up shape for Sym(150) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   V106 = [0  0  0]                                       from Bubble up value for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   S106 = [3]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   V108 = [1  1  1]                                       from Bubble up value for Sym(108) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   S108 = [3]                                             from Bubble up shape for Sym(108) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   V15 = [u228]                                           from Bubble up value for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   V149 = [u173  u172  u171]                              from Bubble up value for Sym(149) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   S149 = [3]                                             from Bubble up shape for Sym(149) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   V15 = [u228]                                           from Bubble up value for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   V106 = [0  0  0]                                       from Bubble up value for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : POST:   S106 = [3]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : PRE:    length(S106) = length([u8976])                         from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : PRE:    S149 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : PRE:    S108 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : PRE:    S106 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // RuntimeCheck : PRE:    V106 < V149                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        // Shape: V153=[u378] and S153=[]
                        // with: With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(149) step=Sym(108) width=Sym(106)  Sym(150) => Sym(152))
                        val lb0: Int = x106.content()(0)
                        val ub0: Int = x149.content()(0)
                        val step0: Int = x108.content()(0)
                        val width0: Int = x106.content()(0)
                        val ll0: Int = if (x15) lb0 + 1 else lb0
                        val ul0: Int = if (x15) ub0 else ub0 + 1
                        for (iv0 <- List.range(ll0, ul0)) {
                          if ((iv0 - lb0) % step0 <= width0) {
                            val lb1: Int = x106.content()(1)
                            val ub1: Int = x149.content()(1)
                            val step1: Int = x108.content()(1)
                            val width1: Int = x106.content()(1)
                            val ll1: Int = if (x15) lb1 + 1 else lb1
                            val ul1: Int = if (x15) ub1 else ub1 + 1
                            for (iv1 <- List.range(ll1, ul1)) {
                              if ((iv1 - lb1) % step1 <= width1) {
                                val lb2: Int = x106.content()(2)
                                val ub2: Int = x149.content()(2)
                                val step2: Int = x108.content()(2)
                                val width2: Int = x106.content()(2)
                                val ll2: Int = if (x15) lb2 + 1 else lb2
                                val ul2: Int = if (x15) ub2 else ub2 + 1
                                for (iv2 <- List.range(ll2, ul2)) {
                                  if ((iv2 - lb2) % step2 <= width2) {
                                    val x150: MDArray[Int] = iv0::iv1::iv2::Nil
                                    val iv: MDArray[Int] = x150
                                    val feval: MDArray[Double] = {
                                      // RuntimeCheck : POST:   V148 = [u385  u384  u383]                              from Bubble up value for Sym(148) <- InfixOp(+: Sym(150) and Sym(148))
                                      // RuntimeCheck : POST:   S148 = [3]                                             from Bubble up shape for Sym(148) <- InfixOp(+: Sym(150) and Sym(148))
                                      // RuntimeCheck : POST:   S150 = [3]                                             from Bubble up shape for Sym(150) <- InfixOp(+: Sym(150) and Sym(148))
                                      // RuntimeCheck : PRE:    S150 = S148 OR S148 = []                               from InfixOp(+: Sym(150) and Sym(148))
                                      // Shape: V151=[u441(<u382)  u442(<u381)  u443(<u380)] and S151=[3]
                                      val x151: MDArray[Int] = {
                                        val result = new Array[Int](shape(x150).content().foldLeft(1)((a,b) => a*b))
                                        for(i <- List.range(0, result.length))
                                        result(i) = x150.content()(i) +  x148.content()(i)
                                        internalReshape(shape(x150), result, "infixOpAA")
                                      }
                                      // RuntimeCheck : POST:   S139 = [u382  u381  u380]                              from Bubble up shape for Sym(139) <- Sel(Sym(151), Sym(139))
                                      // RuntimeCheck : POST:   V151 = [u441(<u382)  u442(<u381)  u443(<u380)]         from Bubble up value for Sym(151) <- Sel(Sym(151), Sym(139))
                                      // RuntimeCheck : POST:   S151 = [3]                                             from Bubble up shape for Sym(151) <- Sel(Sym(151), Sym(139))
                                      // RuntimeCheck : PRE:    length(S151) = length([u9101])                         from Sel(Sym(151), Sym(139))
                                      // RuntimeCheck : PRE:    S139(:length(V151)) < V151                             from Sel(Sym(151), Sym(139))
                                      // Shape: V152=[u379] and S152=[]
                                      
                                      // Shape: V152=[u379] and S152=[]
                                      val x152: Double = x139.content()(flatten(shape(x139), x151, "sel"))
                                      x152
                                    }
                                    // the action of this loop:
                                    if (result == null) {
                                      // create the array and shape
                                      result = new Array[Double](x102.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                      rshape = shape(feval).content()
                                    } else {
                                      // check shape -- this WILL be redundant due to runtime checks
                                      if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                                    }
                                    // copy new content
                                    val mainIndex: Int = flatten(x102 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                                    for (innerIndex <- List.range(0, rshape.length)) {
                                      result(mainIndex + innerIndex) = feval(innerIndex)
                                    }
                                  } // if ((iv0 ...
                                } // for (iv0 ...
                              } // if ((iv1 ...
                            } // for (iv1 ...
                          } // if ((iv2 ...
                        } // for (iv2 ...
                        internalReshape(x102 ::: rshape.toList, result, opName)
                      }
                      
                      // RuntimeCheck : POST:   V154 = [u377  u376  u375  u374  u373  u372  u371  u370  u369  u368  u367  u366  u365  u364  u363  u362  u361  u360  u359  u358  u357  u356  u355  u354  u353  u352  u351]     from Bubble up value for Sym(154) <- InfixOp(*: Sym(98) and Sym(154))
                      // RuntimeCheck : POST:   S154 = [3  3  3]                                       from Bubble up shape for Sym(154) <- InfixOp(*: Sym(98) and Sym(154))
                      // RuntimeCheck : POST:   V98 = [u227  u226  u225  u224  u223  u222  u221  u220  u219  u218  u217  u216  u215  u214  u213  u212  u211  u210  u209  u208  u207  u206  u205  u204  u203  u202  u201]     from Bubble up value for Sym(98) <- InfixOp(*: Sym(98) and Sym(154))
                      // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- InfixOp(*: Sym(98) and Sym(154))
                      // RuntimeCheck : PRE:    S98 = S154 OR S154 = []                                from InfixOp(*: Sym(98) and Sym(154))
                      // Shape: V155=[u200  u199  u198  ...  u176  u175  u174] and S155=[3  3  3]
                      val x155: MDArray[Double] = {
                        val result = new Array[Double](shape(x98).content().foldLeft(1)((a,b) => a*b))
                        for(i <- List.range(0, result.length))
                        result(i) = x98.content()(i) *  x154.content()(i)
                        internalReshape(shape(x98), result, "infixOpAA")
                      }
                      // RuntimeCheck : POST:   V155 = [u200  u199  u198  u197  u196  u195  u194  u193  u192  u191  u190  u189  u188  u187  u186  u185  u184  u183  u182  u181  u180  u179  u178  u177  u176  u175  u174]     from Bubble up value for Sym(155) <- Dim(Sym(155))
                      // RuntimeCheck : POST:   S155 = [3  3  3]                                       from Bubble up shape for Sym(155) <- Dim(Sym(155))
                      // Shape: V156=[3] and S156=[]
                      val x156: Int = dim(x155)
                      // RuntimeCheck : POST:   V156 = [3]                                             from Bubble up value for Sym(156) <- FromValue(Sym(156))
                      // RuntimeCheck : POST:   S156 = []                                              from Bubble up shape for Sym(156) <- FromValue(Sym(156))
                      // Shape: V157=[3] and S157=[]
                      val x157: Int = x156
                      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(157))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(157))
                      // RuntimeCheck : POST:   V157 = [3]                                             from Bubble up value for Sym(157) <- Values(Sym(12), Sym(157))
                      // RuntimeCheck : POST:   S157 = []                                              from Bubble up shape for Sym(157) <- Values(Sym(12), Sym(157))
                      // RuntimeCheck : PRE:    S157 = []                                              from Values(Sym(12), Sym(157))
                      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(157))
                      // Shape: V158=[0  0  0] and S158=[3]
                      val x158: MDArray[Int] = {
                        val result = new Array[Int](x157)
                        for(i <- List.range(0, result.length))
                        result(i) = x12
                        internalReshape(x157::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V155 = [u200  u199  u198  u197  u196  u195  u194  u193  u192  u191  u190  u189  u188  u187  u186  u185  u184  u183  u182  u181  u180  u179  u178  u177  u176  u175  u174]     from Bubble up value for Sym(155) <- Shape(Sym(155))
                      // RuntimeCheck : POST:   S155 = [3  3  3]                                       from Bubble up shape for Sym(155) <- Shape(Sym(155))
                      // Shape: V159=[3  3  3] and S159=[3]
                      val x159: MDArray[Int] = shape(x155)
                      // RuntimeCheck : POST:   V158 = [0  0  0]                                       from Bubble up value for Sym(158) <- Shape(Sym(158))
                      // RuntimeCheck : POST:   S158 = [3]                                             from Bubble up shape for Sym(158) <- Shape(Sym(158))
                      // Shape: V160=[3] and S160=[1]
                      val x160: MDArray[Int] = shape(x158)
                      // RuntimeCheck : POST:   V160 = [3]                                             from Bubble up value for Sym(160) <- Sel(Sym(9), Sym(160))
                      // RuntimeCheck : POST:   S160 = [1]                                             from Bubble up shape for Sym(160) <- Sel(Sym(9), Sym(160))
                      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(160))
                      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(160))
                      // RuntimeCheck : PRE:    length(S9) = length([u7799])                           from Sel(Sym(9), Sym(160))
                      // RuntimeCheck : PRE:    S160(:length(V9)) < V9                                 from Sel(Sym(9), Sym(160))
                      // Shape: V161=[3] and S161=[]
                      
                      // Shape: V161=[3] and S161=[]
                      val x161: Int = x160.content()(flatten(shape(x160), x9, "sel"))
                      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(161))
                      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(161))
                      // RuntimeCheck : POST:   V161 = [3]                                             from Bubble up value for Sym(161) <- Values(Sym(7), Sym(161))
                      // RuntimeCheck : POST:   S161 = []                                              from Bubble up shape for Sym(161) <- Values(Sym(7), Sym(161))
                      // RuntimeCheck : PRE:    S161 = []                                              from Values(Sym(7), Sym(161))
                      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(161))
                      // Shape: V162=[1  1  1] and S162=[3]
                      val x162: MDArray[Int] = {
                        val result = new Array[Int](x161)
                        for(i <- List.range(0, result.length))
                        result(i) = x7
                        internalReshape(x161::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(161))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(161))
                      // RuntimeCheck : POST:   V161 = [3]                                             from Bubble up value for Sym(161) <- Values(Sym(12), Sym(161))
                      // RuntimeCheck : POST:   S161 = []                                              from Bubble up shape for Sym(161) <- Values(Sym(12), Sym(161))
                      // RuntimeCheck : PRE:    S161 = []                                              from Values(Sym(12), Sym(161))
                      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(161))
                      // Shape: V163=[0  0  0] and S163=[3]
                      val x163: MDArray[Int] = {
                        val result = new Array[Int](x161)
                        for(i <- List.range(0, result.length))
                        result(i) = x12
                        internalReshape(x161::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V168 = [u323]                                          from Bubble up value for Sym(168) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   S168 = []                                              from Bubble up shape for Sym(168) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   V167 = [u336(<3)  u337(<3)  u338(<3)]                  from Bubble up value for Sym(167) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   S167 = [3]                                             from Bubble up shape for Sym(167) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   V163 = [0  0  0]                                       from Bubble up value for Sym(163) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   S163 = [3]                                             from Bubble up shape for Sym(163) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   V162 = [1  1  1]                                       from Bubble up value for Sym(162) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   S162 = [3]                                             from Bubble up shape for Sym(162) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   V25 = [u248]                                           from Bubble up value for Sym(25) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   V159 = [3  3  3]                                       from Bubble up value for Sym(159) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   S159 = [3]                                             from Bubble up shape for Sym(159) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   V15 = [u228]                                           from Bubble up value for Sym(15) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   V158 = [0  0  0]                                       from Bubble up value for Sym(158) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : POST:   S158 = [3]                                             from Bubble up shape for Sym(158) <- With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : PRE:    length(S158) = length([u4359])                         from With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : PRE:    S159 = S158                                            from With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : PRE:    S162 = S158                                            from With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : PRE:    S163 = S158                                            from With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // RuntimeCheck : PRE:    V158 < V159                                            from With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      // Shape: V169=[u322] and S169=[]
                      // with: With(lb=Sym(158) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(159) step=Sym(162) width=Sym(163)  Sym(167) => Sym(168))
                      val lb0: Int = x158.content()(0)
                      val ub0: Int = x159.content()(0)
                      val step0: Int = x162.content()(0)
                      val width0: Int = x163.content()(0)
                      val ll0: Int = if (x15) lb0 + 1 else lb0
                      val ul0: Int = if (x25) ub0 else ub0 + 1
                      for (iv0 <- List.range(ll0, ul0)) {
                        if ((iv0 - lb0) % step0 <= width0) {
                          val lb1: Int = x158.content()(1)
                          val ub1: Int = x159.content()(1)
                          val step1: Int = x162.content()(1)
                          val width1: Int = x163.content()(1)
                          val ll1: Int = if (x15) lb1 + 1 else lb1
                          val ul1: Int = if (x25) ub1 else ub1 + 1
                          for (iv1 <- List.range(ll1, ul1)) {
                            if ((iv1 - lb1) % step1 <= width1) {
                              val lb2: Int = x158.content()(2)
                              val ub2: Int = x159.content()(2)
                              val step2: Int = x162.content()(2)
                              val width2: Int = x163.content()(2)
                              val ll2: Int = if (x15) lb2 + 1 else lb2
                              val ul2: Int = if (x25) ub2 else ub2 + 1
                              for (iv2 <- List.range(ll2, ul2)) {
                                if ((iv2 - lb2) % step2 <= width2) {
                                  val x167: MDArray[Int] = iv0::iv1::iv2::Nil
                                  val iv: MDArray[Int] = x167
                                  val feval: MDArray[Double] = {
                                    // RuntimeCheck : POST:   V155 = [u200  u199  u198  u197  u196  u195  u194  u193  u192  u191  u190  u189  u188  u187  u186  u185  u184  u183  u182  u181  u180  u179  u178  u177  u176  u175  u174]     from Bubble up value for Sym(155) <- Sel(Sym(167), Sym(155))
                                    // RuntimeCheck : POST:   S155 = [3  3  3]                                       from Bubble up shape for Sym(155) <- Sel(Sym(167), Sym(155))
                                    // RuntimeCheck : POST:   V167 = [u336(<3)  u337(<3)  u338(<3)]                  from Bubble up value for Sym(167) <- Sel(Sym(167), Sym(155))
                                    // RuntimeCheck : POST:   S167 = [3]                                             from Bubble up shape for Sym(167) <- Sel(Sym(167), Sym(155))
                                    // RuntimeCheck : PRE:    length(S167) = length([u8946])                         from Sel(Sym(167), Sym(155))
                                    // RuntimeCheck : PRE:    S155(:length(V167)) < V167                             from Sel(Sym(167), Sym(155))
                                    // Shape: V168=[u323] and S168=[]
                                    
                                    // Shape: V168=[u323] and S168=[]
                                    val x168: Double = x155.content()(flatten(shape(x155), x167, "sel"))
                                    x168
                                  }
                                  // the action of this loop:
                                  result = foldFunction(result, feval)
                                } // if ((iv0 ...
                              } // for (iv0 ...
                            } // if ((iv1 ...
                          } // for (iv1 ...
                        } // if ((iv2 ...
                      } // for (iv2 ...
                      result
                    }
                    
                    // RuntimeCheck : POST:   V170 = [u233]                                          from Bubble up value for Sym(170) <- ToValue(Sym(170))
                    // RuntimeCheck : POST:   S170 = []                                              from Bubble up shape for Sym(170) <- ToValue(Sym(170))
                    // RuntimeCheck : PRE:    length(S170) = length([])                              from ToValue(Sym(170))
                    // Shape: V171=[u116] and S171=[]
                    val x171: Double = x170
                    // RuntimeCheck : POST:   V171 = [u116]                                          from Bubble up value for Sym(171) <- FromValue(Sym(171))
                    // RuntimeCheck : POST:   S171 = []                                              from Bubble up shape for Sym(171) <- FromValue(Sym(171))
                    // Shape: V174=[u115] and S174=[]
                    val x174: Double = x171
                    // RuntimeCheck : POST:   V174 = [u115]                                          from Bubble up value for Sym(174) <- InfixOp(+: Sym(173) and Sym(174))
                    // RuntimeCheck : POST:   S174 = []                                              from Bubble up shape for Sym(174) <- InfixOp(+: Sym(173) and Sym(174))
                    // RuntimeCheck : POST:   V173 = [u433]                                          from Bubble up value for Sym(173) <- InfixOp(+: Sym(173) and Sym(174))
                    // RuntimeCheck : POST:   S173 = []                                              from Bubble up shape for Sym(173) <- InfixOp(+: Sym(173) and Sym(174))
                    // RuntimeCheck : PRE:    S173 = S174 OR S174 = []                               from InfixOp(+: Sym(173) and Sym(174))
                    // Shape: V175=[u432] and S175=[]
                    val x175: Double = {
                      val result = new Array[Double](shape(x173).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x173.content()(i) +  x174
                      internalReshape(shape(x173), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V49 = [u11]                                            from Bubble up value for Sym(49) <- InfixOp(*: Sym(175) and Sym(49))
                    // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- InfixOp(*: Sym(175) and Sym(49))
                    // RuntimeCheck : POST:   V175 = [u432]                                          from Bubble up value for Sym(175) <- InfixOp(*: Sym(175) and Sym(49))
                    // RuntimeCheck : POST:   S175 = []                                              from Bubble up shape for Sym(175) <- InfixOp(*: Sym(175) and Sym(49))
                    // RuntimeCheck : PRE:    S175 = S49 OR S49 = []                                 from InfixOp(*: Sym(175) and Sym(49))
                    // Shape: V176=[u431] and S176=[]
                    val x176: Double = {
                      val result = new Array[Double](shape(x175).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x175.content()(i) *  x49
                      internalReshape(shape(x175), result, "infixOpAA")
                    }
                    x176
                  }
                  // the action of this loop:
                  if (rshape == null) {
                    rshape = shape(x139).drop(iv.content().length)
                  }
                  val mainIndex: Int = flatten(shape(x139), iv ::: zeros(dim(x139) - iv.content().length), opName)
                  // check shape -- this WILL be redundant due to runtime checks
                  if (shape(feval).content().toList != rshape) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                  // copy new content
                  for (innerIndex <- List.range(0, feval.content().length)) {
                    result(mainIndex + innerIndex) = feval.content()(innerIndex)
                  }
                } // if ((iv0 ...
              } // for (iv0 ...
            } // if ((iv1 ...
          } // for (iv1 ...
        } // if ((iv2 ...
      } // for (iv2 ...
      internalReshape(shape(x139) ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S139 = [u382  u381  u380]                              from Bubble up shape for Sym(139) <- Where(Sym(140), Sym(178), Sym(139))
    // RuntimeCheck : POST:   S178 = [u382  u381  u380]                              from Bubble up shape for Sym(178) <- Where(Sym(140), Sym(178), Sym(139))
    // RuntimeCheck : POST:   S140 = [u382  u381  u380]                              from Bubble up shape for Sym(140) <- Where(Sym(140), Sym(178), Sym(139))
    // RuntimeCheck : PRE:    S140 = S178                                            from Where(Sym(140), Sym(178), Sym(139))
    // RuntimeCheck : PRE:    S140 = S139                                            from Where(Sym(140), Sym(178), Sym(139))
    // Shape: S179=[u382  u381  u380]
    val x179: MDArray[Double] = {
      val result = new Array[Double](shape(x178).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = if (x140.content()(i)) x178.content()(i) else x139.content()(i)
      internalReshape(shape(x178), result, "where")
    }
    x179
  }
}
/*****************************************
  End of Generated Code                  
*******************************************/
