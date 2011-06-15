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
  def apply(x1:scala.virtualization.lms.epfl.test7.original.MDArray[Double]): scala.virtualization.lms.epfl.test7.original.MDArray[Double] = {
    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Shape(Sym(1))
    // Shape: V6=[u143  u142  u141] and S6=[3]
    val x6: MDArray[Int] = shape(x1)
    // RuntimeCheck : POST:   V6 = [u143  u142  u141]                                from Bubble up value for Sym(6) <- GenArrayWith(Sym(6) - Sym(18))
    // RuntimeCheck : POST:   S6 = [3]                                               from Bubble up shape for Sym(6) <- GenArrayWith(Sym(6) - Sym(18))
    // RuntimeCheck : POST:   V18 = [u104]                                           from Bubble up value for Sym(18) <- GenArrayWith(Sym(6) - Sym(18))
    // RuntimeCheck : POST:   S18 = []                                               from Bubble up shape for Sym(18) <- GenArrayWith(Sym(6) - Sym(18))
    // RuntimeCheck : PRE:    S6 = [u659]                                            from GenArrayWith(Sym(6) - Sym(18))
    // RuntimeCheck : PRE:    S6 = S2                                                from GenArrayWith(Sym(6) - Sym(18))
    // RuntimeCheck : PRE:    V6(:length(V2)) < V2                                   from GenArrayWith(Sym(6) - Sym(18))
    // RuntimeCheck : PRE:    V6(length(V2):) = S18                                  from GenArrayWith(Sym(6) - Sym(18))
    // Shape: S19=[u143  u142  u141]
    
    val x19: MDArray[Boolean] = {
      val opName: String = "genarray"
      var result: Array[Boolean] = null
      var rshape: Array[Int] = null
      // Shape: V2=[1  0  0] and S2=[3]
      val x2: MDArray[Int] = internalReshape(3::Nil, Array(1, 0, 0), "knownAtCompileTime")
      // Shape: V3=[2  1  1] and S3=[3]
      val x3: MDArray[Int] = internalReshape(3::Nil, Array(2, 1, 1), "knownAtCompileTime")
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(6) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(6) and Sym(7))
      // RuntimeCheck : POST:   V6 = [u143  u142  u141]                                from Bubble up value for Sym(6) <- InfixOp(-: Sym(6) and Sym(7))
      // RuntimeCheck : POST:   S6 = [3]                                               from Bubble up shape for Sym(6) <- InfixOp(-: Sym(6) and Sym(7))
      // RuntimeCheck : PRE:    S6 = S7 OR S7 = []                                     from InfixOp(-: Sym(6) and Sym(7))
      // Shape: V8=[u160  u159  u158] and S8=[3]
      val x8: MDArray[Int] = {
        val result = new Array[Int](shape(x6).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x6.content()(i) -  x7
        internalReshape(shape(x6), result, "infixOpAA")
      }
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V6 = [u143  u142  u141]                                from Bubble up value for Sym(6) <- Shape(Sym(6))
      // RuntimeCheck : POST:   S6 = [3]                                               from Bubble up shape for Sym(6) <- Shape(Sym(6))
      // Shape: V10=[3] and S10=[1]
      val x10: MDArray[Int] = shape(x6)
      // RuntimeCheck : POST:   V10 = [3]                                              from Bubble up value for Sym(10) <- Sel(Sym(9), Sym(10))
      // RuntimeCheck : POST:   S10 = [1]                                              from Bubble up shape for Sym(10) <- Sel(Sym(9), Sym(10))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(10))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(10))
      // RuntimeCheck : PRE:    length(S9) = length([u680])                            from Sel(Sym(9), Sym(10))
      // RuntimeCheck : PRE:    S10(:length(V9)) < V9                                  from Sel(Sym(9), Sym(10))
      // Shape: V11=[3] and S11=[]
      
      // Shape: V11=[3] and S11=[]
      val x11: Int = x10.content()(flatten(shape(x10), x9, "sel"))
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(11))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(11))
      // RuntimeCheck : POST:   V11 = [3]                                              from Bubble up value for Sym(11) <- Values(Sym(12), Sym(11))
      // RuntimeCheck : POST:   S11 = []                                               from Bubble up shape for Sym(11) <- Values(Sym(12), Sym(11))
      // RuntimeCheck : PRE:    S11 = []                                               from Values(Sym(12), Sym(11))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(11))
      // Shape: V13=[0  0  0] and S13=[3]
      val x13: MDArray[Int] = {
        val result = new Array[Int](x11)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x11::Nil, result, "values")
      }
      // Shape: V4=[u4] and S4=[]
      val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
      // RuntimeCheck : POST:   V4 = [u4]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
      // Shape: V5=[u107] and S5=[]
      val x5: Boolean = x4
      // RuntimeCheck : POST:   V5 = [u107]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
      // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
      // Shape: V14=[u106] and S14=[]
      val x14: Boolean = x5
      // RuntimeCheck : POST:   V14 = [u106]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
      // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
      // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
      // Shape: V15=[u105] and S15=[]
      val x15: Boolean = x14
      // Shape: V17=[u5] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u5]                                             from Bubble up value for Sym(17) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   S16 = [3]                                              from Bubble up shape for Sym(16) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   V13 = [0  0  0]                                        from Bubble up value for Sym(13) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   S13 = [3]                                              from Bubble up shape for Sym(13) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   V3 = [2  1  1]                                         from Bubble up value for Sym(3) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   S3 = [3]                                               from Bubble up shape for Sym(3) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   V15 = [u105]                                           from Bubble up value for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   V8 = [u160  u159  u158]                                from Bubble up value for Sym(8) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   S8 = [3]                                               from Bubble up shape for Sym(8) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   V15 = [u105]                                           from Bubble up value for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   V2 = [1  0  0]                                         from Bubble up value for Sym(2) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : POST:   S2 = [3]                                               from Bubble up shape for Sym(2) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : PRE:    length(S2) = length([u660])                            from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : PRE:    S8 = S2                                                from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : PRE:    S3 = S2                                                from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : PRE:    S13 = S2                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // RuntimeCheck : PRE:    V2 < V8                                                from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      // Shape: V18=[u104] and S18=[]
      // with: With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(8) step=Sym(3) width=Sym(13)  Sym(16) => Sym(17))
      val lb0: Int = x2.content()(0)
      val ub0: Int = x8.content()(0)
      val step0: Int = x3.content()(0)
      val width0: Int = x13.content()(0)
      val ll0: Int = if (x15) lb0 + 1 else lb0
      val ul0: Int = if (x15) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x2.content()(1)
          val ub1: Int = x8.content()(1)
          val step1: Int = x3.content()(1)
          val width1: Int = x13.content()(1)
          val ll1: Int = if (x15) lb1 + 1 else lb1
          val ul1: Int = if (x15) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x2.content()(2)
              val ub2: Int = x8.content()(2)
              val step2: Int = x3.content()(2)
              val width2: Int = x13.content()(2)
              val ll2: Int = if (x15) lb2 + 1 else lb2
              val ul2: Int = if (x15) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x16: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x16
                  val feval: MDArray[Boolean] = {
                    x17
                  }
                  // the action of this loop:
                  if (result == null) {
                    // create the array and shape
                    result = new Array[Boolean](x6.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                    rshape = shape(feval).content()
                  } else {
                    // check shape -- this WILL be redundant due to runtime checks
                    if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                  }
                  // copy new content
                  val mainIndex: Int = flatten(x6 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                  for (innerIndex <- List.range(0, rshape.length)) {
                    result(mainIndex + innerIndex) = feval(innerIndex)
                  }
                } // if ((iv0 ...
              } // for (iv0 ...
            } // if ((iv1 ...
          } // for (iv1 ...
        } // if ((iv2 ...
      } // for (iv2 ...
      internalReshape(x6 ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- ModArrayWith(Sym(1) - Sym(51))
    // RuntimeCheck : POST:   V51 = [u228]                                           from Bubble up value for Sym(51) <- ModArrayWith(Sym(1) - Sym(51))
    // RuntimeCheck : POST:   S51 = []                                               from Bubble up shape for Sym(51) <- ModArrayWith(Sym(1) - Sym(51))
    // RuntimeCheck : PRE:    S13 = [u669]                                           from ModArrayWith(Sym(1) - Sym(51))
    // RuntimeCheck : PRE:    S13 = [LengthOf(S1)]                                   from ModArrayWith(Sym(1) - Sym(51))
    // RuntimeCheck : PRE:    S1(:length(V13)) < V13                                 from ModArrayWith(Sym(1) - Sym(51))
    // RuntimeCheck : PRE:    S1(length(V13):) = S51                                 from ModArrayWith(Sym(1) - Sym(51))
    // Shape: S52=[u143  u142  u141]
    
    val x52: MDArray[Double] = {
      val opName: String = "modarray"
      var result: Array[Double] = new Array[Double](shape(x1).content().foldLeft(1)((a,b) => a*b))
      for (i <- List.range(0, result.length)) {
        result(i) = x1.content()(i)
      }
      var rshape: List[Int] = null
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V6 = [u143  u142  u141]                                from Bubble up value for Sym(6) <- Shape(Sym(6))
      // RuntimeCheck : POST:   S6 = [3]                                               from Bubble up shape for Sym(6) <- Shape(Sym(6))
      // Shape: V10=[3] and S10=[1]
      val x10: MDArray[Int] = shape(x6)
      // RuntimeCheck : POST:   V10 = [3]                                              from Bubble up value for Sym(10) <- Sel(Sym(9), Sym(10))
      // RuntimeCheck : POST:   S10 = [1]                                              from Bubble up shape for Sym(10) <- Sel(Sym(9), Sym(10))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(10))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(10))
      // RuntimeCheck : PRE:    length(S9) = length([u680])                            from Sel(Sym(9), Sym(10))
      // RuntimeCheck : PRE:    S10(:length(V9)) < V9                                  from Sel(Sym(9), Sym(10))
      // Shape: V11=[3] and S11=[]
      
      // Shape: V11=[3] and S11=[]
      val x11: Int = x10.content()(flatten(shape(x10), x9, "sel"))
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(11))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(11))
      // RuntimeCheck : POST:   V11 = [3]                                              from Bubble up value for Sym(11) <- Values(Sym(12), Sym(11))
      // RuntimeCheck : POST:   S11 = []                                               from Bubble up shape for Sym(11) <- Values(Sym(12), Sym(11))
      // RuntimeCheck : PRE:    S11 = []                                               from Values(Sym(12), Sym(11))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(11))
      // Shape: V13=[0  0  0] and S13=[3]
      val x13: MDArray[Int] = {
        val result = new Array[Int](x11)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x11::Nil, result, "values")
      }
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(6) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(6) and Sym(7))
      // RuntimeCheck : POST:   V6 = [u143  u142  u141]                                from Bubble up value for Sym(6) <- InfixOp(-: Sym(6) and Sym(7))
      // RuntimeCheck : POST:   S6 = [3]                                               from Bubble up shape for Sym(6) <- InfixOp(-: Sym(6) and Sym(7))
      // RuntimeCheck : PRE:    S6 = S7 OR S7 = []                                     from InfixOp(-: Sym(6) and Sym(7))
      // Shape: V22=[u157  u156  u155] and S22=[3]
      val x22: MDArray[Int] = {
        val result = new Array[Int](shape(x6).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x6.content()(i) -  x7
        internalReshape(shape(x6), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(11))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(11))
      // RuntimeCheck : POST:   V11 = [3]                                              from Bubble up value for Sym(11) <- Values(Sym(7), Sym(11))
      // RuntimeCheck : POST:   S11 = []                                               from Bubble up shape for Sym(11) <- Values(Sym(7), Sym(11))
      // RuntimeCheck : PRE:    S11 = []                                               from Values(Sym(7), Sym(11))
      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(11))
      // Shape: V23=[1  1  1] and S23=[3]
      val x23: MDArray[Int] = {
        val result = new Array[Int](x11)
        for(i <- List.range(0, result.length))
        result(i) = x7
        internalReshape(x11::Nil, result, "values")
      }
      // Shape: V17=[u5] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u5]                                             from Bubble up value for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : PRE:    length(S17) = length([])                               from ToValue(Sym(17))
      // Shape: V21=[u77] and S21=[]
      val x21: Boolean = x17
      // RuntimeCheck : POST:   V21 = [u77]                                            from Bubble up value for Sym(21) <- FromValue(Sym(21))
      // RuntimeCheck : POST:   S21 = []                                               from Bubble up shape for Sym(21) <- FromValue(Sym(21))
      // Shape: V24=[u76] and S24=[]
      val x24: Boolean = x21
      // RuntimeCheck : POST:   V24 = [u76]                                            from Bubble up value for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : PRE:    length(S24) = length([])                               from ToValue(Sym(24))
      // Shape: V25=[u75] and S25=[]
      val x25: Boolean = x24
      // Shape: V2=[1  0  0] and S2=[3]
      val x2: MDArray[Int] = internalReshape(3::Nil, Array(1, 0, 0), "knownAtCompileTime")
      // Shape: V32=[0  1  0] and S32=[3]
      val x32: MDArray[Int] = internalReshape(3::Nil, Array(0, 1, 0), "knownAtCompileTime")
      // Shape: V39=[0  0  1] and S39=[3]
      val x39: MDArray[Int] = internalReshape(3::Nil, Array(0, 0, 1), "knownAtCompileTime")
      // Shape: V20=[u11] and S20=[]
      val x20: Double = internalReshape(Nil, Array(0.1), "knownAtCompileTime")
      // Shape: V49=[u12] and S49=[]
      val x49: Double = internalReshape(Nil, Array(0.16666666666666666), "knownAtCompileTime")
      // RuntimeCheck : POST:   V50 = [u229]                                           from Bubble up value for Sym(50) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   S50 = []                                               from Bubble up shape for Sym(50) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   V26 = [u239(<u143)  u240(<u142)  u241(<u141)]          from Bubble up value for Sym(26) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   S26 = [3]                                              from Bubble up shape for Sym(26) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   V13 = [0  0  0]                                        from Bubble up value for Sym(13) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   S13 = [3]                                              from Bubble up shape for Sym(13) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   V23 = [1  1  1]                                        from Bubble up value for Sym(23) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   S23 = [3]                                              from Bubble up shape for Sym(23) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   V25 = [u75]                                            from Bubble up value for Sym(25) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   V22 = [u157  u156  u155]                               from Bubble up value for Sym(22) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   S22 = [3]                                              from Bubble up shape for Sym(22) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   V25 = [u75]                                            from Bubble up value for Sym(25) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   V13 = [0  0  0]                                        from Bubble up value for Sym(13) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : POST:   S13 = [3]                                              from Bubble up shape for Sym(13) <- With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : PRE:    length(S13) = length([u670])                           from With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : PRE:    S22 = S13                                              from With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : PRE:    S23 = S13                                              from With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : PRE:    S13 = S13                                              from With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // RuntimeCheck : PRE:    V13 < V22                                              from With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      // Shape: V51=[u228] and S51=[]
      // with: With(lb=Sym(13) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(22) step=Sym(23) width=Sym(13)  Sym(26) => Sym(50))
      val lb0: Int = x13.content()(0)
      val ub0: Int = x22.content()(0)
      val step0: Int = x23.content()(0)
      val width0: Int = x13.content()(0)
      val ll0: Int = if (x25) lb0 + 1 else lb0
      val ul0: Int = if (x25) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x13.content()(1)
          val ub1: Int = x22.content()(1)
          val step1: Int = x23.content()(1)
          val width1: Int = x13.content()(1)
          val ll1: Int = if (x25) lb1 + 1 else lb1
          val ul1: Int = if (x25) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x13.content()(2)
              val ub2: Int = x22.content()(2)
              val step2: Int = x23.content()(2)
              val width2: Int = x13.content()(2)
              val ll2: Int = if (x25) lb2 + 1 else lb2
              val ul2: Int = if (x25) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x26: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x26
                  val feval: MDArray[Double] = {
                    // RuntimeCheck : POST:   V2 = [1  0  0]                                         from Bubble up value for Sym(2) <- InfixOp(+: Sym(26) and Sym(2))
                    // RuntimeCheck : POST:   S2 = [3]                                               from Bubble up shape for Sym(2) <- InfixOp(+: Sym(26) and Sym(2))
                    // RuntimeCheck : POST:   V26 = [u239(<u143)  u240(<u142)  u241(<u141)]          from Bubble up value for Sym(26) <- InfixOp(+: Sym(26) and Sym(2))
                    // RuntimeCheck : POST:   S26 = [3]                                              from Bubble up shape for Sym(26) <- InfixOp(+: Sym(26) and Sym(2))
                    // RuntimeCheck : PRE:    S26 = S2 OR S2 = []                                    from InfixOp(+: Sym(26) and Sym(2))
                    // Shape: V27=[u257(<u143)  u258(<u142)  u259(<u141)] and S27=[3]
                    val x27: MDArray[Int] = {
                      val result = new Array[Int](shape(x26).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x26.content()(i) +  x2.content()(i)
                      internalReshape(shape(x26), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Sel(Sym(27), Sym(1))
                    // RuntimeCheck : POST:   V27 = [u257(<u143)  u258(<u142)  u259(<u141)]          from Bubble up value for Sym(27) <- Sel(Sym(27), Sym(1))
                    // RuntimeCheck : POST:   S27 = [3]                                              from Bubble up shape for Sym(27) <- Sel(Sym(27), Sym(1))
                    // RuntimeCheck : PRE:    length(S27) = length([u685])                           from Sel(Sym(27), Sym(1))
                    // RuntimeCheck : PRE:    S1(:length(V27)) < V27                                 from Sel(Sym(27), Sym(1))
                    // Shape: V28=[u221] and S28=[]
                    
                    // Shape: V28=[u221] and S28=[]
                    val x28: Double = x1.content()(flatten(shape(x1), x27, "sel"))
                    // RuntimeCheck : POST:   V2 = [1  0  0]                                         from Bubble up value for Sym(2) <- InfixOp(-: Sym(26) and Sym(2))
                    // RuntimeCheck : POST:   S2 = [3]                                               from Bubble up shape for Sym(2) <- InfixOp(-: Sym(26) and Sym(2))
                    // RuntimeCheck : POST:   V26 = [u239(<u143)  u240(<u142)  u241(<u141)]          from Bubble up value for Sym(26) <- InfixOp(-: Sym(26) and Sym(2))
                    // RuntimeCheck : POST:   S26 = [3]                                              from Bubble up shape for Sym(26) <- InfixOp(-: Sym(26) and Sym(2))
                    // RuntimeCheck : PRE:    S26 = S2 OR S2 = []                                    from InfixOp(-: Sym(26) and Sym(2))
                    // Shape: V29=[u254(<u143)  u255(<u142)  u256(<u141)] and S29=[3]
                    val x29: MDArray[Int] = {
                      val result = new Array[Int](shape(x26).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x26.content()(i) -  x2.content()(i)
                      internalReshape(shape(x26), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Sel(Sym(29), Sym(1))
                    // RuntimeCheck : POST:   V29 = [u254(<u143)  u255(<u142)  u256(<u141)]          from Bubble up value for Sym(29) <- Sel(Sym(29), Sym(1))
                    // RuntimeCheck : POST:   S29 = [3]                                              from Bubble up shape for Sym(29) <- Sel(Sym(29), Sym(1))
                    // RuntimeCheck : PRE:    length(S29) = length([u686])                           from Sel(Sym(29), Sym(1))
                    // RuntimeCheck : PRE:    S1(:length(V29)) < V29                                 from Sel(Sym(29), Sym(1))
                    // Shape: V30=[u212] and S30=[]
                    
                    // Shape: V30=[u212] and S30=[]
                    val x30: Double = x1.content()(flatten(shape(x1), x29, "sel"))
                    // RuntimeCheck : POST:   V30 = [u212]                                           from Bubble up value for Sym(30) <- InfixOp(+: Sym(28) and Sym(30))
                    // RuntimeCheck : POST:   S30 = []                                               from Bubble up shape for Sym(30) <- InfixOp(+: Sym(28) and Sym(30))
                    // RuntimeCheck : POST:   V28 = [u221]                                           from Bubble up value for Sym(28) <- InfixOp(+: Sym(28) and Sym(30))
                    // RuntimeCheck : POST:   S28 = []                                               from Bubble up shape for Sym(28) <- InfixOp(+: Sym(28) and Sym(30))
                    // RuntimeCheck : PRE:    S28 = S30 OR S30 = []                                  from InfixOp(+: Sym(28) and Sym(30))
                    // Shape: V31=[u220] and S31=[]
                    val x31: Double = {
                      val result = new Array[Double](shape(x28).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x28.content()(i) +  x30
                      internalReshape(shape(x28), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V32 = [0  1  0]                                        from Bubble up value for Sym(32) <- InfixOp(+: Sym(26) and Sym(32))
                    // RuntimeCheck : POST:   S32 = [3]                                              from Bubble up shape for Sym(32) <- InfixOp(+: Sym(26) and Sym(32))
                    // RuntimeCheck : POST:   V26 = [u239(<u143)  u240(<u142)  u241(<u141)]          from Bubble up value for Sym(26) <- InfixOp(+: Sym(26) and Sym(32))
                    // RuntimeCheck : POST:   S26 = [3]                                              from Bubble up shape for Sym(26) <- InfixOp(+: Sym(26) and Sym(32))
                    // RuntimeCheck : PRE:    S26 = S32 OR S32 = []                                  from InfixOp(+: Sym(26) and Sym(32))
                    // Shape: V33=[u251(<u143)  u252(<u142)  u253(<u141)] and S33=[3]
                    val x33: MDArray[Int] = {
                      val result = new Array[Int](shape(x26).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x26.content()(i) +  x32.content()(i)
                      internalReshape(shape(x26), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Sel(Sym(33), Sym(1))
                    // RuntimeCheck : POST:   V33 = [u251(<u143)  u252(<u142)  u253(<u141)]          from Bubble up value for Sym(33) <- Sel(Sym(33), Sym(1))
                    // RuntimeCheck : POST:   S33 = [3]                                              from Bubble up shape for Sym(33) <- Sel(Sym(33), Sym(1))
                    // RuntimeCheck : PRE:    length(S33) = length([u687])                           from Sel(Sym(33), Sym(1))
                    // RuntimeCheck : PRE:    S1(:length(V33)) < V33                                 from Sel(Sym(33), Sym(1))
                    // Shape: V34=[u208] and S34=[]
                    
                    // Shape: V34=[u208] and S34=[]
                    val x34: Double = x1.content()(flatten(shape(x1), x33, "sel"))
                    // RuntimeCheck : POST:   V34 = [u208]                                           from Bubble up value for Sym(34) <- InfixOp(+: Sym(31) and Sym(34))
                    // RuntimeCheck : POST:   S34 = []                                               from Bubble up shape for Sym(34) <- InfixOp(+: Sym(31) and Sym(34))
                    // RuntimeCheck : POST:   V31 = [u220]                                           from Bubble up value for Sym(31) <- InfixOp(+: Sym(31) and Sym(34))
                    // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- InfixOp(+: Sym(31) and Sym(34))
                    // RuntimeCheck : PRE:    S31 = S34 OR S34 = []                                  from InfixOp(+: Sym(31) and Sym(34))
                    // Shape: V35=[u219] and S35=[]
                    val x35: Double = {
                      val result = new Array[Double](shape(x31).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x31.content()(i) +  x34
                      internalReshape(shape(x31), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V32 = [0  1  0]                                        from Bubble up value for Sym(32) <- InfixOp(-: Sym(26) and Sym(32))
                    // RuntimeCheck : POST:   S32 = [3]                                              from Bubble up shape for Sym(32) <- InfixOp(-: Sym(26) and Sym(32))
                    // RuntimeCheck : POST:   V26 = [u239(<u143)  u240(<u142)  u241(<u141)]          from Bubble up value for Sym(26) <- InfixOp(-: Sym(26) and Sym(32))
                    // RuntimeCheck : POST:   S26 = [3]                                              from Bubble up shape for Sym(26) <- InfixOp(-: Sym(26) and Sym(32))
                    // RuntimeCheck : PRE:    S26 = S32 OR S32 = []                                  from InfixOp(-: Sym(26) and Sym(32))
                    // Shape: V36=[u248(<u143)  u249(<u142)  u250(<u141)] and S36=[3]
                    val x36: MDArray[Int] = {
                      val result = new Array[Int](shape(x26).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x26.content()(i) -  x32.content()(i)
                      internalReshape(shape(x26), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Sel(Sym(36), Sym(1))
                    // RuntimeCheck : POST:   V36 = [u248(<u143)  u249(<u142)  u250(<u141)]          from Bubble up value for Sym(36) <- Sel(Sym(36), Sym(1))
                    // RuntimeCheck : POST:   S36 = [3]                                              from Bubble up shape for Sym(36) <- Sel(Sym(36), Sym(1))
                    // RuntimeCheck : PRE:    length(S36) = length([u688])                           from Sel(Sym(36), Sym(1))
                    // RuntimeCheck : PRE:    S1(:length(V36)) < V36                                 from Sel(Sym(36), Sym(1))
                    // Shape: V37=[u204] and S37=[]
                    
                    // Shape: V37=[u204] and S37=[]
                    val x37: Double = x1.content()(flatten(shape(x1), x36, "sel"))
                    // RuntimeCheck : POST:   V37 = [u204]                                           from Bubble up value for Sym(37) <- InfixOp(+: Sym(35) and Sym(37))
                    // RuntimeCheck : POST:   S37 = []                                               from Bubble up shape for Sym(37) <- InfixOp(+: Sym(35) and Sym(37))
                    // RuntimeCheck : POST:   V35 = [u219]                                           from Bubble up value for Sym(35) <- InfixOp(+: Sym(35) and Sym(37))
                    // RuntimeCheck : POST:   S35 = []                                               from Bubble up shape for Sym(35) <- InfixOp(+: Sym(35) and Sym(37))
                    // RuntimeCheck : PRE:    S35 = S37 OR S37 = []                                  from InfixOp(+: Sym(35) and Sym(37))
                    // Shape: V38=[u218] and S38=[]
                    val x38: Double = {
                      val result = new Array[Double](shape(x35).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x35.content()(i) +  x37
                      internalReshape(shape(x35), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V39 = [0  0  1]                                        from Bubble up value for Sym(39) <- InfixOp(+: Sym(26) and Sym(39))
                    // RuntimeCheck : POST:   S39 = [3]                                              from Bubble up shape for Sym(39) <- InfixOp(+: Sym(26) and Sym(39))
                    // RuntimeCheck : POST:   V26 = [u239(<u143)  u240(<u142)  u241(<u141)]          from Bubble up value for Sym(26) <- InfixOp(+: Sym(26) and Sym(39))
                    // RuntimeCheck : POST:   S26 = [3]                                              from Bubble up shape for Sym(26) <- InfixOp(+: Sym(26) and Sym(39))
                    // RuntimeCheck : PRE:    S26 = S39 OR S39 = []                                  from InfixOp(+: Sym(26) and Sym(39))
                    // Shape: V40=[u245(<u143)  u246(<u142)  u247(<u141)] and S40=[3]
                    val x40: MDArray[Int] = {
                      val result = new Array[Int](shape(x26).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x26.content()(i) +  x39.content()(i)
                      internalReshape(shape(x26), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Sel(Sym(40), Sym(1))
                    // RuntimeCheck : POST:   V40 = [u245(<u143)  u246(<u142)  u247(<u141)]          from Bubble up value for Sym(40) <- Sel(Sym(40), Sym(1))
                    // RuntimeCheck : POST:   S40 = [3]                                              from Bubble up shape for Sym(40) <- Sel(Sym(40), Sym(1))
                    // RuntimeCheck : PRE:    length(S40) = length([u689])                           from Sel(Sym(40), Sym(1))
                    // RuntimeCheck : PRE:    S1(:length(V40)) < V40                                 from Sel(Sym(40), Sym(1))
                    // Shape: V41=[u200] and S41=[]
                    
                    // Shape: V41=[u200] and S41=[]
                    val x41: Double = x1.content()(flatten(shape(x1), x40, "sel"))
                    // RuntimeCheck : POST:   V41 = [u200]                                           from Bubble up value for Sym(41) <- InfixOp(+: Sym(38) and Sym(41))
                    // RuntimeCheck : POST:   S41 = []                                               from Bubble up shape for Sym(41) <- InfixOp(+: Sym(38) and Sym(41))
                    // RuntimeCheck : POST:   V38 = [u218]                                           from Bubble up value for Sym(38) <- InfixOp(+: Sym(38) and Sym(41))
                    // RuntimeCheck : POST:   S38 = []                                               from Bubble up shape for Sym(38) <- InfixOp(+: Sym(38) and Sym(41))
                    // RuntimeCheck : PRE:    S38 = S41 OR S41 = []                                  from InfixOp(+: Sym(38) and Sym(41))
                    // Shape: V42=[u217] and S42=[]
                    val x42: Double = {
                      val result = new Array[Double](shape(x38).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x38.content()(i) +  x41
                      internalReshape(shape(x38), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V39 = [0  0  1]                                        from Bubble up value for Sym(39) <- InfixOp(-: Sym(26) and Sym(39))
                    // RuntimeCheck : POST:   S39 = [3]                                              from Bubble up shape for Sym(39) <- InfixOp(-: Sym(26) and Sym(39))
                    // RuntimeCheck : POST:   V26 = [u239(<u143)  u240(<u142)  u241(<u141)]          from Bubble up value for Sym(26) <- InfixOp(-: Sym(26) and Sym(39))
                    // RuntimeCheck : POST:   S26 = [3]                                              from Bubble up shape for Sym(26) <- InfixOp(-: Sym(26) and Sym(39))
                    // RuntimeCheck : PRE:    S26 = S39 OR S39 = []                                  from InfixOp(-: Sym(26) and Sym(39))
                    // Shape: V43=[u242(<u143)  u243(<u142)  u244(<u141)] and S43=[3]
                    val x43: MDArray[Int] = {
                      val result = new Array[Int](shape(x26).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x26.content()(i) -  x39.content()(i)
                      internalReshape(shape(x26), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Sel(Sym(43), Sym(1))
                    // RuntimeCheck : POST:   V43 = [u242(<u143)  u243(<u142)  u244(<u141)]          from Bubble up value for Sym(43) <- Sel(Sym(43), Sym(1))
                    // RuntimeCheck : POST:   S43 = [3]                                              from Bubble up shape for Sym(43) <- Sel(Sym(43), Sym(1))
                    // RuntimeCheck : PRE:    length(S43) = length([u690])                           from Sel(Sym(43), Sym(1))
                    // RuntimeCheck : PRE:    S1(:length(V43)) < V43                                 from Sel(Sym(43), Sym(1))
                    // Shape: V44=[u196] and S44=[]
                    
                    // Shape: V44=[u196] and S44=[]
                    val x44: Double = x1.content()(flatten(shape(x1), x43, "sel"))
                    // RuntimeCheck : POST:   V44 = [u196]                                           from Bubble up value for Sym(44) <- InfixOp(+: Sym(42) and Sym(44))
                    // RuntimeCheck : POST:   S44 = []                                               from Bubble up shape for Sym(44) <- InfixOp(+: Sym(42) and Sym(44))
                    // RuntimeCheck : POST:   V42 = [u217]                                           from Bubble up value for Sym(42) <- InfixOp(+: Sym(42) and Sym(44))
                    // RuntimeCheck : POST:   S42 = []                                               from Bubble up shape for Sym(42) <- InfixOp(+: Sym(42) and Sym(44))
                    // RuntimeCheck : PRE:    S42 = S44 OR S44 = []                                  from InfixOp(+: Sym(42) and Sym(44))
                    // Shape: V45=[u216] and S45=[]
                    val x45: Double = {
                      val result = new Array[Double](shape(x42).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x42.content()(i) +  x44
                      internalReshape(shape(x42), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Sel(Sym(26), Sym(1))
                    // RuntimeCheck : POST:   V26 = [u239(<u143)  u240(<u142)  u241(<u141)]          from Bubble up value for Sym(26) <- Sel(Sym(26), Sym(1))
                    // RuntimeCheck : POST:   S26 = [3]                                              from Bubble up shape for Sym(26) <- Sel(Sym(26), Sym(1))
                    // RuntimeCheck : PRE:    length(S26) = length([u683])                           from Sel(Sym(26), Sym(1))
                    // RuntimeCheck : PRE:    S1(:length(V26)) < V26                                 from Sel(Sym(26), Sym(1))
                    // Shape: V46=[u232] and S46=[]
                    
                    // Shape: V46=[u232] and S46=[]
                    val x46: Double = x1.content()(flatten(shape(x1), x26, "sel"))
                    // RuntimeCheck : POST:   V20 = [u11]                                            from Bubble up value for Sym(20) <- InfixOp(*: Sym(46) and Sym(20))
                    // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(*: Sym(46) and Sym(20))
                    // RuntimeCheck : POST:   V46 = [u232]                                           from Bubble up value for Sym(46) <- InfixOp(*: Sym(46) and Sym(20))
                    // RuntimeCheck : POST:   S46 = []                                               from Bubble up shape for Sym(46) <- InfixOp(*: Sym(46) and Sym(20))
                    // RuntimeCheck : PRE:    S46 = S20 OR S20 = []                                  from InfixOp(*: Sym(46) and Sym(20))
                    // Shape: V47=[u231] and S47=[]
                    val x47: Double = {
                      val result = new Array[Double](shape(x46).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x46.content()(i) *  x20
                      internalReshape(shape(x46), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V45 = [u216]                                           from Bubble up value for Sym(45) <- InfixOp(+: Sym(47) and Sym(45))
                    // RuntimeCheck : POST:   S45 = []                                               from Bubble up shape for Sym(45) <- InfixOp(+: Sym(47) and Sym(45))
                    // RuntimeCheck : POST:   V47 = [u231]                                           from Bubble up value for Sym(47) <- InfixOp(+: Sym(47) and Sym(45))
                    // RuntimeCheck : POST:   S47 = []                                               from Bubble up shape for Sym(47) <- InfixOp(+: Sym(47) and Sym(45))
                    // RuntimeCheck : PRE:    S47 = S45 OR S45 = []                                  from InfixOp(+: Sym(47) and Sym(45))
                    // Shape: V48=[u230] and S48=[]
                    val x48: Double = {
                      val result = new Array[Double](shape(x47).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x47.content()(i) +  x45
                      internalReshape(shape(x47), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V49 = [u12]                                            from Bubble up value for Sym(49) <- InfixOp(*: Sym(48) and Sym(49))
                    // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- InfixOp(*: Sym(48) and Sym(49))
                    // RuntimeCheck : POST:   V48 = [u230]                                           from Bubble up value for Sym(48) <- InfixOp(*: Sym(48) and Sym(49))
                    // RuntimeCheck : POST:   S48 = []                                               from Bubble up shape for Sym(48) <- InfixOp(*: Sym(48) and Sym(49))
                    // RuntimeCheck : PRE:    S48 = S49 OR S49 = []                                  from InfixOp(*: Sym(48) and Sym(49))
                    // Shape: V50=[u229] and S50=[]
                    val x50: Double = {
                      val result = new Array[Double](shape(x48).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x48.content()(i) *  x49
                      internalReshape(shape(x48), result, "infixOpAA")
                    }
                    x50
                  }
                  // the action of this loop:
                  if (rshape == null) {
                    rshape = shape(x1).drop(iv.content().length)
                  }
                  val mainIndex: Int = flatten(shape(x1), iv ::: zeros(dim(x1) - iv.content().length), opName)
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
      internalReshape(shape(x1) ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Where(Sym(19), Sym(52), Sym(1))
    // RuntimeCheck : POST:   S52 = [u143  u142  u141]                               from Bubble up shape for Sym(52) <- Where(Sym(19), Sym(52), Sym(1))
    // RuntimeCheck : POST:   S19 = [u143  u142  u141]                               from Bubble up shape for Sym(19) <- Where(Sym(19), Sym(52), Sym(1))
    // RuntimeCheck : PRE:    S19 = S52                                              from Where(Sym(19), Sym(52), Sym(1))
    // RuntimeCheck : PRE:    S19 = S1                                               from Where(Sym(19), Sym(52), Sym(1))
    // Shape: S53=[u143  u142  u141]
    val x53: MDArray[Double] = {
      val result = new Array[Double](shape(x52).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = if (x19.content()(i)) x52.content()(i) else x1.content()(i)
      internalReshape(shape(x52), result, "where")
    }
    // RuntimeCheck : POST:   S19 = [u143  u142  u141]                               from Bubble up shape for Sym(19) <- UnaryOp(!: Sym(19))
    // Shape: S54=[u143  u142  u141]
    val x54: MDArray[Boolean] = {
      val result = new Array[Boolean](shape(x19).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = !x19.content()(i)
      internalReshape(shape(x19), result, "unaryOp")
    }
    // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- ModArrayWith(Sym(53) - Sym(83))
    // RuntimeCheck : POST:   V83 = [u161]                                           from Bubble up value for Sym(83) <- ModArrayWith(Sym(53) - Sym(83))
    // RuntimeCheck : POST:   S83 = []                                               from Bubble up shape for Sym(83) <- ModArrayWith(Sym(53) - Sym(83))
    // RuntimeCheck : PRE:    S58 = [u273]                                           from ModArrayWith(Sym(53) - Sym(83))
    // RuntimeCheck : PRE:    S58 = [LengthOf(S53)]                                  from ModArrayWith(Sym(53) - Sym(83))
    // RuntimeCheck : PRE:    S53(:length(V58)) < V58                                from ModArrayWith(Sym(53) - Sym(83))
    // RuntimeCheck : PRE:    S53(length(V58):) = S83                                from ModArrayWith(Sym(53) - Sym(83))
    // Shape: S84=[u143  u142  u141]
    
    val x84: MDArray[Double] = {
      val opName: String = "modarray"
      var result: Array[Double] = new Array[Double](shape(x53).content().foldLeft(1)((a,b) => a*b))
      for (i <- List.range(0, result.length)) {
        result(i) = x53.content()(i)
      }
      var rshape: List[Int] = null
      // Shape: V17=[u5] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u5]                                             from Bubble up value for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : PRE:    length(S17) = length([])                               from ToValue(Sym(17))
      // Shape: V21=[u77] and S21=[]
      val x21: Boolean = x17
      // RuntimeCheck : POST:   V21 = [u77]                                            from Bubble up value for Sym(21) <- FromValue(Sym(21))
      // RuntimeCheck : POST:   S21 = []                                               from Bubble up shape for Sym(21) <- FromValue(Sym(21))
      // Shape: V24=[u76] and S24=[]
      val x24: Boolean = x21
      // RuntimeCheck : POST:   V24 = [u76]                                            from Bubble up value for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : PRE:    length(S24) = length([])                               from ToValue(Sym(24))
      // Shape: V25=[u75] and S25=[]
      val x25: Boolean = x24
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- Shape(Sym(53))
      // Shape: V55=[u143  u142  u141] and S55=[3]
      val x55: MDArray[Int] = shape(x53)
      // RuntimeCheck : POST:   V55 = [u143  u142  u141]                               from Bubble up value for Sym(55) <- Shape(Sym(55))
      // RuntimeCheck : POST:   S55 = [3]                                              from Bubble up shape for Sym(55) <- Shape(Sym(55))
      // Shape: V56=[3] and S56=[1]
      val x56: MDArray[Int] = shape(x55)
      // RuntimeCheck : POST:   V56 = [3]                                              from Bubble up value for Sym(56) <- Sel(Sym(9), Sym(56))
      // RuntimeCheck : POST:   S56 = [1]                                              from Bubble up shape for Sym(56) <- Sel(Sym(9), Sym(56))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(56))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(56))
      // RuntimeCheck : PRE:    length(S9) = length([u383])                            from Sel(Sym(9), Sym(56))
      // RuntimeCheck : PRE:    S56(:length(V9)) < V9                                  from Sel(Sym(9), Sym(56))
      // Shape: V57=[3] and S57=[]
      
      // Shape: V57=[3] and S57=[]
      val x57: Int = x56.content()(flatten(shape(x56), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(57))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(57))
      // RuntimeCheck : POST:   V57 = [3]                                              from Bubble up value for Sym(57) <- Values(Sym(12), Sym(57))
      // RuntimeCheck : POST:   S57 = []                                               from Bubble up shape for Sym(57) <- Values(Sym(12), Sym(57))
      // RuntimeCheck : PRE:    S57 = []                                               from Values(Sym(12), Sym(57))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(57))
      // Shape: V58=[0  0  0] and S58=[3]
      val x58: MDArray[Int] = {
        val result = new Array[Int](x57)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x57::Nil, result, "values")
      }
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(55) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(55) and Sym(7))
      // RuntimeCheck : POST:   V55 = [u143  u142  u141]                               from Bubble up value for Sym(55) <- InfixOp(-: Sym(55) and Sym(7))
      // RuntimeCheck : POST:   S55 = [3]                                              from Bubble up shape for Sym(55) <- InfixOp(-: Sym(55) and Sym(7))
      // RuntimeCheck : PRE:    S55 = S7 OR S7 = []                                    from InfixOp(-: Sym(55) and Sym(7))
      // Shape: V59=[u137  u136  u135] and S59=[3]
      val x59: MDArray[Int] = {
        val result = new Array[Int](shape(x55).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x55.content()(i) -  x7
        internalReshape(shape(x55), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(57))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(57))
      // RuntimeCheck : POST:   V57 = [3]                                              from Bubble up value for Sym(57) <- Values(Sym(7), Sym(57))
      // RuntimeCheck : POST:   S57 = []                                               from Bubble up shape for Sym(57) <- Values(Sym(7), Sym(57))
      // RuntimeCheck : PRE:    S57 = []                                               from Values(Sym(7), Sym(57))
      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(57))
      // Shape: V60=[1  1  1] and S60=[3]
      val x60: MDArray[Int] = {
        val result = new Array[Int](x57)
        for(i <- List.range(0, result.length))
        result(i) = x7
        internalReshape(x57::Nil, result, "values")
      }
      // Shape: V49=[u12] and S49=[]
      val x49: Double = internalReshape(Nil, Array(0.16666666666666666), "knownAtCompileTime")
      // Shape: V2=[1  0  0] and S2=[3]
      val x2: MDArray[Int] = internalReshape(3::Nil, Array(1, 0, 0), "knownAtCompileTime")
      // Shape: V32=[0  1  0] and S32=[3]
      val x32: MDArray[Int] = internalReshape(3::Nil, Array(0, 1, 0), "knownAtCompileTime")
      // Shape: V39=[0  0  1] and S39=[3]
      val x39: MDArray[Int] = internalReshape(3::Nil, Array(0, 0, 1), "knownAtCompileTime")
      // Shape: V20=[u11] and S20=[]
      val x20: Double = internalReshape(Nil, Array(0.1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V82 = [u162]                                           from Bubble up value for Sym(82) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   S82 = []                                               from Bubble up shape for Sym(82) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   V61 = [u172(<u143)  u173(<u142)  u174(<u141)]          from Bubble up value for Sym(61) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   S61 = [3]                                              from Bubble up shape for Sym(61) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   V58 = [0  0  0]                                        from Bubble up value for Sym(58) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   S58 = [3]                                              from Bubble up shape for Sym(58) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   V60 = [1  1  1]                                        from Bubble up value for Sym(60) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   S60 = [3]                                              from Bubble up shape for Sym(60) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   V25 = [u75]                                            from Bubble up value for Sym(25) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   V59 = [u137  u136  u135]                               from Bubble up value for Sym(59) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   S59 = [3]                                              from Bubble up shape for Sym(59) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   V25 = [u75]                                            from Bubble up value for Sym(25) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   V58 = [0  0  0]                                        from Bubble up value for Sym(58) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : POST:   S58 = [3]                                              from Bubble up shape for Sym(58) <- With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : PRE:    length(S58) = length([u274])                           from With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : PRE:    S59 = S58                                              from With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : PRE:    S60 = S58                                              from With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : PRE:    S58 = S58                                              from With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // RuntimeCheck : PRE:    V58 < V59                                              from With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      // Shape: V83=[u161] and S83=[]
      // with: With(lb=Sym(58) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(59) step=Sym(60) width=Sym(58)  Sym(61) => Sym(82))
      val lb0: Int = x58.content()(0)
      val ub0: Int = x59.content()(0)
      val step0: Int = x60.content()(0)
      val width0: Int = x58.content()(0)
      val ll0: Int = if (x25) lb0 + 1 else lb0
      val ul0: Int = if (x25) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x58.content()(1)
          val ub1: Int = x59.content()(1)
          val step1: Int = x60.content()(1)
          val width1: Int = x58.content()(1)
          val ll1: Int = if (x25) lb1 + 1 else lb1
          val ul1: Int = if (x25) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x58.content()(2)
              val ub2: Int = x59.content()(2)
              val step2: Int = x60.content()(2)
              val width2: Int = x58.content()(2)
              val ll2: Int = if (x25) lb2 + 1 else lb2
              val ul2: Int = if (x25) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x61: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x61
                  val feval: MDArray[Double] = {
                    // RuntimeCheck : POST:   V2 = [1  0  0]                                         from Bubble up value for Sym(2) <- InfixOp(+: Sym(61) and Sym(2))
                    // RuntimeCheck : POST:   S2 = [3]                                               from Bubble up shape for Sym(2) <- InfixOp(+: Sym(61) and Sym(2))
                    // RuntimeCheck : POST:   V61 = [u172(<u143)  u173(<u142)  u174(<u141)]          from Bubble up value for Sym(61) <- InfixOp(+: Sym(61) and Sym(2))
                    // RuntimeCheck : POST:   S61 = [3]                                              from Bubble up shape for Sym(61) <- InfixOp(+: Sym(61) and Sym(2))
                    // RuntimeCheck : PRE:    S61 = S2 OR S2 = []                                    from InfixOp(+: Sym(61) and Sym(2))
                    // Shape: V62=[u190(<u143)  u191(<u142)  u192(<u141)] and S62=[3]
                    val x62: MDArray[Int] = {
                      val result = new Array[Int](shape(x61).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x61.content()(i) +  x2.content()(i)
                      internalReshape(shape(x61), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- Sel(Sym(62), Sym(53))
                    // RuntimeCheck : POST:   V62 = [u190(<u143)  u191(<u142)  u192(<u141)]          from Bubble up value for Sym(62) <- Sel(Sym(62), Sym(53))
                    // RuntimeCheck : POST:   S62 = [3]                                              from Bubble up shape for Sym(62) <- Sel(Sym(62), Sym(53))
                    // RuntimeCheck : PRE:    length(S62) = length([u421])                           from Sel(Sym(62), Sym(53))
                    // RuntimeCheck : PRE:    S53(:length(V62)) < V62                                from Sel(Sym(62), Sym(53))
                    // Shape: V63=[u154] and S63=[]
                    
                    // Shape: V63=[u154] and S63=[]
                    val x63: Double = x53.content()(flatten(shape(x53), x62, "sel"))
                    // RuntimeCheck : POST:   V2 = [1  0  0]                                         from Bubble up value for Sym(2) <- InfixOp(-: Sym(61) and Sym(2))
                    // RuntimeCheck : POST:   S2 = [3]                                               from Bubble up shape for Sym(2) <- InfixOp(-: Sym(61) and Sym(2))
                    // RuntimeCheck : POST:   V61 = [u172(<u143)  u173(<u142)  u174(<u141)]          from Bubble up value for Sym(61) <- InfixOp(-: Sym(61) and Sym(2))
                    // RuntimeCheck : POST:   S61 = [3]                                              from Bubble up shape for Sym(61) <- InfixOp(-: Sym(61) and Sym(2))
                    // RuntimeCheck : PRE:    S61 = S2 OR S2 = []                                    from InfixOp(-: Sym(61) and Sym(2))
                    // Shape: V64=[u187(<u143)  u188(<u142)  u189(<u141)] and S64=[3]
                    val x64: MDArray[Int] = {
                      val result = new Array[Int](shape(x61).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x61.content()(i) -  x2.content()(i)
                      internalReshape(shape(x61), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- Sel(Sym(64), Sym(53))
                    // RuntimeCheck : POST:   V64 = [u187(<u143)  u188(<u142)  u189(<u141)]          from Bubble up value for Sym(64) <- Sel(Sym(64), Sym(53))
                    // RuntimeCheck : POST:   S64 = [3]                                              from Bubble up shape for Sym(64) <- Sel(Sym(64), Sym(53))
                    // RuntimeCheck : PRE:    length(S64) = length([u455])                           from Sel(Sym(64), Sym(53))
                    // RuntimeCheck : PRE:    S53(:length(V64)) < V64                                from Sel(Sym(64), Sym(53))
                    // Shape: V65=[u148] and S65=[]
                    
                    // Shape: V65=[u148] and S65=[]
                    val x65: Double = x53.content()(flatten(shape(x53), x64, "sel"))
                    // RuntimeCheck : POST:   V65 = [u148]                                           from Bubble up value for Sym(65) <- InfixOp(+: Sym(63) and Sym(65))
                    // RuntimeCheck : POST:   S65 = []                                               from Bubble up shape for Sym(65) <- InfixOp(+: Sym(63) and Sym(65))
                    // RuntimeCheck : POST:   V63 = [u154]                                           from Bubble up value for Sym(63) <- InfixOp(+: Sym(63) and Sym(65))
                    // RuntimeCheck : POST:   S63 = []                                               from Bubble up shape for Sym(63) <- InfixOp(+: Sym(63) and Sym(65))
                    // RuntimeCheck : PRE:    S63 = S65 OR S65 = []                                  from InfixOp(+: Sym(63) and Sym(65))
                    // Shape: V66=[u153] and S66=[]
                    val x66: Double = {
                      val result = new Array[Double](shape(x63).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x63.content()(i) +  x65
                      internalReshape(shape(x63), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V32 = [0  1  0]                                        from Bubble up value for Sym(32) <- InfixOp(+: Sym(61) and Sym(32))
                    // RuntimeCheck : POST:   S32 = [3]                                              from Bubble up shape for Sym(32) <- InfixOp(+: Sym(61) and Sym(32))
                    // RuntimeCheck : POST:   V61 = [u172(<u143)  u173(<u142)  u174(<u141)]          from Bubble up value for Sym(61) <- InfixOp(+: Sym(61) and Sym(32))
                    // RuntimeCheck : POST:   S61 = [3]                                              from Bubble up shape for Sym(61) <- InfixOp(+: Sym(61) and Sym(32))
                    // RuntimeCheck : PRE:    S61 = S32 OR S32 = []                                  from InfixOp(+: Sym(61) and Sym(32))
                    // Shape: V67=[u184(<u143)  u185(<u142)  u186(<u141)] and S67=[3]
                    val x67: MDArray[Int] = {
                      val result = new Array[Int](shape(x61).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x61.content()(i) +  x32.content()(i)
                      internalReshape(shape(x61), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- Sel(Sym(67), Sym(53))
                    // RuntimeCheck : POST:   V67 = [u184(<u143)  u185(<u142)  u186(<u141)]          from Bubble up value for Sym(67) <- Sel(Sym(67), Sym(53))
                    // RuntimeCheck : POST:   S67 = [3]                                              from Bubble up shape for Sym(67) <- Sel(Sym(67), Sym(53))
                    // RuntimeCheck : PRE:    length(S67) = length([u489])                           from Sel(Sym(67), Sym(53))
                    // RuntimeCheck : PRE:    S53(:length(V67)) < V67                                from Sel(Sym(67), Sym(53))
                    // Shape: V68=[u147] and S68=[]
                    
                    // Shape: V68=[u147] and S68=[]
                    val x68: Double = x53.content()(flatten(shape(x53), x67, "sel"))
                    // RuntimeCheck : POST:   V68 = [u147]                                           from Bubble up value for Sym(68) <- InfixOp(+: Sym(66) and Sym(68))
                    // RuntimeCheck : POST:   S68 = []                                               from Bubble up shape for Sym(68) <- InfixOp(+: Sym(66) and Sym(68))
                    // RuntimeCheck : POST:   V66 = [u153]                                           from Bubble up value for Sym(66) <- InfixOp(+: Sym(66) and Sym(68))
                    // RuntimeCheck : POST:   S66 = []                                               from Bubble up shape for Sym(66) <- InfixOp(+: Sym(66) and Sym(68))
                    // RuntimeCheck : PRE:    S66 = S68 OR S68 = []                                  from InfixOp(+: Sym(66) and Sym(68))
                    // Shape: V69=[u152] and S69=[]
                    val x69: Double = {
                      val result = new Array[Double](shape(x66).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x66.content()(i) +  x68
                      internalReshape(shape(x66), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V32 = [0  1  0]                                        from Bubble up value for Sym(32) <- InfixOp(-: Sym(61) and Sym(32))
                    // RuntimeCheck : POST:   S32 = [3]                                              from Bubble up shape for Sym(32) <- InfixOp(-: Sym(61) and Sym(32))
                    // RuntimeCheck : POST:   V61 = [u172(<u143)  u173(<u142)  u174(<u141)]          from Bubble up value for Sym(61) <- InfixOp(-: Sym(61) and Sym(32))
                    // RuntimeCheck : POST:   S61 = [3]                                              from Bubble up shape for Sym(61) <- InfixOp(-: Sym(61) and Sym(32))
                    // RuntimeCheck : PRE:    S61 = S32 OR S32 = []                                  from InfixOp(-: Sym(61) and Sym(32))
                    // Shape: V70=[u181(<u143)  u182(<u142)  u183(<u141)] and S70=[3]
                    val x70: MDArray[Int] = {
                      val result = new Array[Int](shape(x61).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x61.content()(i) -  x32.content()(i)
                      internalReshape(shape(x61), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- Sel(Sym(70), Sym(53))
                    // RuntimeCheck : POST:   V70 = [u181(<u143)  u182(<u142)  u183(<u141)]          from Bubble up value for Sym(70) <- Sel(Sym(70), Sym(53))
                    // RuntimeCheck : POST:   S70 = [3]                                              from Bubble up shape for Sym(70) <- Sel(Sym(70), Sym(53))
                    // RuntimeCheck : PRE:    length(S70) = length([u523])                           from Sel(Sym(70), Sym(53))
                    // RuntimeCheck : PRE:    S53(:length(V70)) < V70                                from Sel(Sym(70), Sym(53))
                    // Shape: V71=[u146] and S71=[]
                    
                    // Shape: V71=[u146] and S71=[]
                    val x71: Double = x53.content()(flatten(shape(x53), x70, "sel"))
                    // RuntimeCheck : POST:   V71 = [u146]                                           from Bubble up value for Sym(71) <- InfixOp(+: Sym(69) and Sym(71))
                    // RuntimeCheck : POST:   S71 = []                                               from Bubble up shape for Sym(71) <- InfixOp(+: Sym(69) and Sym(71))
                    // RuntimeCheck : POST:   V69 = [u152]                                           from Bubble up value for Sym(69) <- InfixOp(+: Sym(69) and Sym(71))
                    // RuntimeCheck : POST:   S69 = []                                               from Bubble up shape for Sym(69) <- InfixOp(+: Sym(69) and Sym(71))
                    // RuntimeCheck : PRE:    S69 = S71 OR S71 = []                                  from InfixOp(+: Sym(69) and Sym(71))
                    // Shape: V72=[u151] and S72=[]
                    val x72: Double = {
                      val result = new Array[Double](shape(x69).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x69.content()(i) +  x71
                      internalReshape(shape(x69), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V39 = [0  0  1]                                        from Bubble up value for Sym(39) <- InfixOp(+: Sym(61) and Sym(39))
                    // RuntimeCheck : POST:   S39 = [3]                                              from Bubble up shape for Sym(39) <- InfixOp(+: Sym(61) and Sym(39))
                    // RuntimeCheck : POST:   V61 = [u172(<u143)  u173(<u142)  u174(<u141)]          from Bubble up value for Sym(61) <- InfixOp(+: Sym(61) and Sym(39))
                    // RuntimeCheck : POST:   S61 = [3]                                              from Bubble up shape for Sym(61) <- InfixOp(+: Sym(61) and Sym(39))
                    // RuntimeCheck : PRE:    S61 = S39 OR S39 = []                                  from InfixOp(+: Sym(61) and Sym(39))
                    // Shape: V73=[u178(<u143)  u179(<u142)  u180(<u141)] and S73=[3]
                    val x73: MDArray[Int] = {
                      val result = new Array[Int](shape(x61).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x61.content()(i) +  x39.content()(i)
                      internalReshape(shape(x61), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- Sel(Sym(73), Sym(53))
                    // RuntimeCheck : POST:   V73 = [u178(<u143)  u179(<u142)  u180(<u141)]          from Bubble up value for Sym(73) <- Sel(Sym(73), Sym(53))
                    // RuntimeCheck : POST:   S73 = [3]                                              from Bubble up shape for Sym(73) <- Sel(Sym(73), Sym(53))
                    // RuntimeCheck : PRE:    length(S73) = length([u557])                           from Sel(Sym(73), Sym(53))
                    // RuntimeCheck : PRE:    S53(:length(V73)) < V73                                from Sel(Sym(73), Sym(53))
                    // Shape: V74=[u145] and S74=[]
                    
                    // Shape: V74=[u145] and S74=[]
                    val x74: Double = x53.content()(flatten(shape(x53), x73, "sel"))
                    // RuntimeCheck : POST:   V74 = [u145]                                           from Bubble up value for Sym(74) <- InfixOp(+: Sym(72) and Sym(74))
                    // RuntimeCheck : POST:   S74 = []                                               from Bubble up shape for Sym(74) <- InfixOp(+: Sym(72) and Sym(74))
                    // RuntimeCheck : POST:   V72 = [u151]                                           from Bubble up value for Sym(72) <- InfixOp(+: Sym(72) and Sym(74))
                    // RuntimeCheck : POST:   S72 = []                                               from Bubble up shape for Sym(72) <- InfixOp(+: Sym(72) and Sym(74))
                    // RuntimeCheck : PRE:    S72 = S74 OR S74 = []                                  from InfixOp(+: Sym(72) and Sym(74))
                    // Shape: V75=[u150] and S75=[]
                    val x75: Double = {
                      val result = new Array[Double](shape(x72).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x72.content()(i) +  x74
                      internalReshape(shape(x72), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V39 = [0  0  1]                                        from Bubble up value for Sym(39) <- InfixOp(-: Sym(61) and Sym(39))
                    // RuntimeCheck : POST:   S39 = [3]                                              from Bubble up shape for Sym(39) <- InfixOp(-: Sym(61) and Sym(39))
                    // RuntimeCheck : POST:   V61 = [u172(<u143)  u173(<u142)  u174(<u141)]          from Bubble up value for Sym(61) <- InfixOp(-: Sym(61) and Sym(39))
                    // RuntimeCheck : POST:   S61 = [3]                                              from Bubble up shape for Sym(61) <- InfixOp(-: Sym(61) and Sym(39))
                    // RuntimeCheck : PRE:    S61 = S39 OR S39 = []                                  from InfixOp(-: Sym(61) and Sym(39))
                    // Shape: V76=[u175(<u143)  u176(<u142)  u177(<u141)] and S76=[3]
                    val x76: MDArray[Int] = {
                      val result = new Array[Int](shape(x61).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x61.content()(i) -  x39.content()(i)
                      internalReshape(shape(x61), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- Sel(Sym(76), Sym(53))
                    // RuntimeCheck : POST:   V76 = [u175(<u143)  u176(<u142)  u177(<u141)]          from Bubble up value for Sym(76) <- Sel(Sym(76), Sym(53))
                    // RuntimeCheck : POST:   S76 = [3]                                              from Bubble up shape for Sym(76) <- Sel(Sym(76), Sym(53))
                    // RuntimeCheck : PRE:    length(S76) = length([u591])                           from Sel(Sym(76), Sym(53))
                    // RuntimeCheck : PRE:    S53(:length(V76)) < V76                                from Sel(Sym(76), Sym(53))
                    // Shape: V77=[u144] and S77=[]
                    
                    // Shape: V77=[u144] and S77=[]
                    val x77: Double = x53.content()(flatten(shape(x53), x76, "sel"))
                    // RuntimeCheck : POST:   V77 = [u144]                                           from Bubble up value for Sym(77) <- InfixOp(+: Sym(75) and Sym(77))
                    // RuntimeCheck : POST:   S77 = []                                               from Bubble up shape for Sym(77) <- InfixOp(+: Sym(75) and Sym(77))
                    // RuntimeCheck : POST:   V75 = [u150]                                           from Bubble up value for Sym(75) <- InfixOp(+: Sym(75) and Sym(77))
                    // RuntimeCheck : POST:   S75 = []                                               from Bubble up shape for Sym(75) <- InfixOp(+: Sym(75) and Sym(77))
                    // RuntimeCheck : PRE:    S75 = S77 OR S77 = []                                  from InfixOp(+: Sym(75) and Sym(77))
                    // Shape: V78=[u149] and S78=[]
                    val x78: Double = {
                      val result = new Array[Double](shape(x75).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x75.content()(i) +  x77
                      internalReshape(shape(x75), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   S1 = [u143  u142  u141]                                from Bubble up shape for Sym(1) <- Sel(Sym(61), Sym(1))
                    // RuntimeCheck : POST:   V61 = [u172(<u143)  u173(<u142)  u174(<u141)]          from Bubble up value for Sym(61) <- Sel(Sym(61), Sym(1))
                    // RuntimeCheck : POST:   S61 = [3]                                              from Bubble up shape for Sym(61) <- Sel(Sym(61), Sym(1))
                    // RuntimeCheck : PRE:    length(S61) = length([u419])                           from Sel(Sym(61), Sym(1))
                    // RuntimeCheck : PRE:    S1(:length(V61)) < V61                                 from Sel(Sym(61), Sym(1))
                    // Shape: V79=[u165] and S79=[]
                    
                    // Shape: V79=[u165] and S79=[]
                    val x79: Double = x1.content()(flatten(shape(x1), x61, "sel"))
                    // RuntimeCheck : POST:   V20 = [u11]                                            from Bubble up value for Sym(20) <- InfixOp(*: Sym(79) and Sym(20))
                    // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(*: Sym(79) and Sym(20))
                    // RuntimeCheck : POST:   V79 = [u165]                                           from Bubble up value for Sym(79) <- InfixOp(*: Sym(79) and Sym(20))
                    // RuntimeCheck : POST:   S79 = []                                               from Bubble up shape for Sym(79) <- InfixOp(*: Sym(79) and Sym(20))
                    // RuntimeCheck : PRE:    S79 = S20 OR S20 = []                                  from InfixOp(*: Sym(79) and Sym(20))
                    // Shape: V80=[u164] and S80=[]
                    val x80: Double = {
                      val result = new Array[Double](shape(x79).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x79.content()(i) *  x20
                      internalReshape(shape(x79), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V78 = [u149]                                           from Bubble up value for Sym(78) <- InfixOp(+: Sym(80) and Sym(78))
                    // RuntimeCheck : POST:   S78 = []                                               from Bubble up shape for Sym(78) <- InfixOp(+: Sym(80) and Sym(78))
                    // RuntimeCheck : POST:   V80 = [u164]                                           from Bubble up value for Sym(80) <- InfixOp(+: Sym(80) and Sym(78))
                    // RuntimeCheck : POST:   S80 = []                                               from Bubble up shape for Sym(80) <- InfixOp(+: Sym(80) and Sym(78))
                    // RuntimeCheck : PRE:    S80 = S78 OR S78 = []                                  from InfixOp(+: Sym(80) and Sym(78))
                    // Shape: V81=[u163] and S81=[]
                    val x81: Double = {
                      val result = new Array[Double](shape(x80).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x80.content()(i) +  x78
                      internalReshape(shape(x80), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V49 = [u12]                                            from Bubble up value for Sym(49) <- InfixOp(*: Sym(81) and Sym(49))
                    // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- InfixOp(*: Sym(81) and Sym(49))
                    // RuntimeCheck : POST:   V81 = [u163]                                           from Bubble up value for Sym(81) <- InfixOp(*: Sym(81) and Sym(49))
                    // RuntimeCheck : POST:   S81 = []                                               from Bubble up shape for Sym(81) <- InfixOp(*: Sym(81) and Sym(49))
                    // RuntimeCheck : PRE:    S81 = S49 OR S49 = []                                  from InfixOp(*: Sym(81) and Sym(49))
                    // Shape: V82=[u162] and S82=[]
                    val x82: Double = {
                      val result = new Array[Double](shape(x81).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x81.content()(i) *  x49
                      internalReshape(shape(x81), result, "infixOpAA")
                    }
                    x82
                  }
                  // the action of this loop:
                  if (rshape == null) {
                    rshape = shape(x53).drop(iv.content().length)
                  }
                  val mainIndex: Int = flatten(shape(x53), iv ::: zeros(dim(x53) - iv.content().length), opName)
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
      internalReshape(shape(x53) ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S53 = [u143  u142  u141]                               from Bubble up shape for Sym(53) <- Where(Sym(54), Sym(84), Sym(53))
    // RuntimeCheck : POST:   S84 = [u143  u142  u141]                               from Bubble up shape for Sym(84) <- Where(Sym(54), Sym(84), Sym(53))
    // RuntimeCheck : POST:   S54 = [u143  u142  u141]                               from Bubble up shape for Sym(54) <- Where(Sym(54), Sym(84), Sym(53))
    // RuntimeCheck : PRE:    S54 = S84                                              from Where(Sym(54), Sym(84), Sym(53))
    // RuntimeCheck : PRE:    S54 = S53                                              from Where(Sym(54), Sym(84), Sym(53))
    // Shape: S85=[u143  u142  u141]
    val x85: MDArray[Double] = {
      val result = new Array[Double](shape(x84).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = if (x54.content()(i)) x84.content()(i) else x53.content()(i)
      internalReshape(shape(x84), result, "where")
    }
    x85
  }
}
/*****************************************
  End of Generated Code                  
*******************************************/
