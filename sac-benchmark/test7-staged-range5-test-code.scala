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
  def apply(x274:scala.virtualization.lms.epfl.test7.original.MDArray[Double]): scala.virtualization.lms.epfl.test7.original.MDArray[Double] = {
    // RuntimeCheck : POST:   S274 = [u409  u408  u407]                              from Bubble up shape for Sym(274) <- Shape(Sym(274))
    // Shape: V275=[u409  u408  u407] and S275=[3]
    val x275: MDArray[Int] = shape(x274)
    // RuntimeCheck : POST:   V275 = [u409  u408  u407]                              from Bubble up value for Sym(275) <- GenArrayWith(Sym(275) - Sym(281))
    // RuntimeCheck : POST:   S275 = [3]                                             from Bubble up shape for Sym(275) <- GenArrayWith(Sym(275) - Sym(281))
    // RuntimeCheck : POST:   V281 = [u364]                                          from Bubble up value for Sym(281) <- GenArrayWith(Sym(275) - Sym(281))
    // RuntimeCheck : POST:   S281 = []                                              from Bubble up shape for Sym(281) <- GenArrayWith(Sym(275) - Sym(281))
    // RuntimeCheck : PRE:    S275 = [u4989]                                         from GenArrayWith(Sym(275) - Sym(281))
    // RuntimeCheck : PRE:    S275 = S2                                              from GenArrayWith(Sym(275) - Sym(281))
    // RuntimeCheck : PRE:    V275(:length(V2)) < V2                                 from GenArrayWith(Sym(275) - Sym(281))
    // RuntimeCheck : PRE:    V275(length(V2):) = S281                               from GenArrayWith(Sym(275) - Sym(281))
    // Shape: S282=[u409  u408  u407]
    
    val x282: MDArray[Boolean] = {
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
      // Shape: V5=[u261] and S5=[]
      val x5: Boolean = x4
      // RuntimeCheck : POST:   V5 = [u261]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
      // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
      // Shape: V14=[u260] and S14=[]
      val x14: Boolean = x5
      // RuntimeCheck : POST:   V14 = [u260]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
      // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
      // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
      // Shape: V15=[u259] and S15=[]
      val x15: Boolean = x14
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(275) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(275) and Sym(7))
      // RuntimeCheck : POST:   V275 = [u409  u408  u407]                              from Bubble up value for Sym(275) <- InfixOp(-: Sym(275) and Sym(7))
      // RuntimeCheck : POST:   S275 = [3]                                             from Bubble up shape for Sym(275) <- InfixOp(-: Sym(275) and Sym(7))
      // RuntimeCheck : PRE:    S275 = S7 OR S7 = []                                   from InfixOp(-: Sym(275) and Sym(7))
      // Shape: V276=[u421  u420  u419] and S276=[3]
      val x276: MDArray[Int] = {
        val result = new Array[Int](shape(x275).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x275.content()(i) -  x7
        internalReshape(shape(x275), result, "infixOpAA")
      }
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V275 = [u409  u408  u407]                              from Bubble up value for Sym(275) <- Shape(Sym(275))
      // RuntimeCheck : POST:   S275 = [3]                                             from Bubble up shape for Sym(275) <- Shape(Sym(275))
      // Shape: V277=[3] and S277=[1]
      val x277: MDArray[Int] = shape(x275)
      // RuntimeCheck : POST:   V277 = [3]                                             from Bubble up value for Sym(277) <- Sel(Sym(9), Sym(277))
      // RuntimeCheck : POST:   S277 = [1]                                             from Bubble up shape for Sym(277) <- Sel(Sym(9), Sym(277))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(277))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(277))
      // RuntimeCheck : PRE:    length(S9) = length([u5148])                           from Sel(Sym(9), Sym(277))
      // RuntimeCheck : PRE:    S277(:length(V9)) < V9                                 from Sel(Sym(9), Sym(277))
      // Shape: V278=[3] and S278=[]
      
      // Shape: V278=[3] and S278=[]
      val x278: Int = x277.content()(flatten(shape(x277), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(278))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(278))
      // RuntimeCheck : POST:   V278 = [3]                                             from Bubble up value for Sym(278) <- Values(Sym(12), Sym(278))
      // RuntimeCheck : POST:   S278 = []                                              from Bubble up shape for Sym(278) <- Values(Sym(12), Sym(278))
      // RuntimeCheck : PRE:    S278 = []                                              from Values(Sym(12), Sym(278))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(278))
      // Shape: V279=[0  0  0] and S279=[3]
      val x279: MDArray[Int] = {
        val result = new Array[Int](x278)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x278::Nil, result, "values")
      }
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   S280 = [3]                                             from Bubble up shape for Sym(280) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   V279 = [0  0  0]                                       from Bubble up value for Sym(279) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   S279 = [3]                                             from Bubble up shape for Sym(279) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   V3 = [2  1  1]                                         from Bubble up value for Sym(3) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   S3 = [3]                                               from Bubble up shape for Sym(3) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   V276 = [u421  u420  u419]                              from Bubble up value for Sym(276) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   S276 = [3]                                             from Bubble up shape for Sym(276) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   V2 = [1  0  0]                                         from Bubble up value for Sym(2) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : POST:   S2 = [3]                                               from Bubble up shape for Sym(2) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : PRE:    length(S2) = length([u4990])                           from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : PRE:    S276 = S2                                              from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : PRE:    S3 = S2                                                from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : PRE:    S279 = S2                                              from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // RuntimeCheck : PRE:    V2 < V276                                              from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      // Shape: V281=[u364] and S281=[]
      // with: With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(276) step=Sym(3) width=Sym(279)  Sym(280) => Sym(17))
      val lb0: Int = x2.content()(0)
      val ub0: Int = x276.content()(0)
      val step0: Int = x3.content()(0)
      val width0: Int = x279.content()(0)
      val ll0: Int = if (x15) lb0 + 1 else lb0
      val ul0: Int = if (x15) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x2.content()(1)
          val ub1: Int = x276.content()(1)
          val step1: Int = x3.content()(1)
          val width1: Int = x279.content()(1)
          val ll1: Int = if (x15) lb1 + 1 else lb1
          val ul1: Int = if (x15) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x2.content()(2)
              val ub2: Int = x276.content()(2)
              val step2: Int = x3.content()(2)
              val width2: Int = x279.content()(2)
              val ll2: Int = if (x15) lb2 + 1 else lb2
              val ul2: Int = if (x15) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x280: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x280
                  val feval: MDArray[Boolean] = {
                    x17
                  }
                  // the action of this loop:
                  if (result == null) {
                    // create the array and shape
                    result = new Array[Boolean](x275.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                    rshape = shape(feval).content()
                  } else {
                    // check shape -- this WILL be redundant due to runtime checks
                    if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                  }
                  // copy new content
                  val mainIndex: Int = flatten(x275 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                  for (innerIndex <- List.range(0, rshape.length)) {
                    result(mainIndex + innerIndex) = feval(innerIndex)
                  }
                } // if ((iv0 ...
              } // for (iv0 ...
            } // if ((iv1 ...
          } // for (iv1 ...
        } // if ((iv2 ...
      } // for (iv2 ...
      internalReshape(x275 ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S274 = [u409  u408  u407]                              from Bubble up shape for Sym(274) <- ModArrayWith(Sym(274) - Sym(353))
    // RuntimeCheck : POST:   V353 = [u480]                                          from Bubble up value for Sym(353) <- ModArrayWith(Sym(274) - Sym(353))
    // RuntimeCheck : POST:   S353 = []                                              from Bubble up shape for Sym(353) <- ModArrayWith(Sym(274) - Sym(353))
    // RuntimeCheck : PRE:    S279 = [u4999]                                         from ModArrayWith(Sym(274) - Sym(353))
    // RuntimeCheck : PRE:    S279 = [LengthOf(S274)]                                from ModArrayWith(Sym(274) - Sym(353))
    // RuntimeCheck : PRE:    S274(:length(V279)) < V279                             from ModArrayWith(Sym(274) - Sym(353))
    // RuntimeCheck : PRE:    S274(length(V279):) = S353                             from ModArrayWith(Sym(274) - Sym(353))
    // Shape: S354=[u409  u408  u407]
    
    val x354: MDArray[Double] = {
      val opName: String = "modarray"
      var result: Array[Double] = new Array[Double](shape(x274).content().foldLeft(1)((a,b) => a*b))
      for (i <- List.range(0, result.length)) {
        result(i) = x274.content()(i)
      }
      var rshape: List[Int] = null
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V275 = [u409  u408  u407]                              from Bubble up value for Sym(275) <- Shape(Sym(275))
      // RuntimeCheck : POST:   S275 = [3]                                             from Bubble up shape for Sym(275) <- Shape(Sym(275))
      // Shape: V277=[3] and S277=[1]
      val x277: MDArray[Int] = shape(x275)
      // RuntimeCheck : POST:   V277 = [3]                                             from Bubble up value for Sym(277) <- Sel(Sym(9), Sym(277))
      // RuntimeCheck : POST:   S277 = [1]                                             from Bubble up shape for Sym(277) <- Sel(Sym(9), Sym(277))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(277))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(277))
      // RuntimeCheck : PRE:    length(S9) = length([u5148])                           from Sel(Sym(9), Sym(277))
      // RuntimeCheck : PRE:    S277(:length(V9)) < V9                                 from Sel(Sym(9), Sym(277))
      // Shape: V278=[3] and S278=[]
      
      // Shape: V278=[3] and S278=[]
      val x278: Int = x277.content()(flatten(shape(x277), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(278))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(278))
      // RuntimeCheck : POST:   V278 = [3]                                             from Bubble up value for Sym(278) <- Values(Sym(12), Sym(278))
      // RuntimeCheck : POST:   S278 = []                                              from Bubble up shape for Sym(278) <- Values(Sym(12), Sym(278))
      // RuntimeCheck : PRE:    S278 = []                                              from Values(Sym(12), Sym(278))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(278))
      // Shape: V279=[0  0  0] and S279=[3]
      val x279: MDArray[Int] = {
        val result = new Array[Int](x278)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x278::Nil, result, "values")
      }
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : PRE:    length(S17) = length([])                               from ToValue(Sym(17))
      // Shape: V21=[u282] and S21=[]
      val x21: Boolean = x17
      // RuntimeCheck : POST:   V21 = [u282]                                           from Bubble up value for Sym(21) <- FromValue(Sym(21))
      // RuntimeCheck : POST:   S21 = []                                               from Bubble up shape for Sym(21) <- FromValue(Sym(21))
      // Shape: V24=[u281] and S24=[]
      val x24: Boolean = x21
      // RuntimeCheck : POST:   V24 = [u281]                                           from Bubble up value for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : PRE:    length(S24) = length([])                               from ToValue(Sym(24))
      // Shape: V25=[u280] and S25=[]
      val x25: Boolean = x24
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(278))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(278))
      // RuntimeCheck : POST:   V278 = [3]                                             from Bubble up value for Sym(278) <- Values(Sym(7), Sym(278))
      // RuntimeCheck : POST:   S278 = []                                              from Bubble up shape for Sym(278) <- Values(Sym(7), Sym(278))
      // RuntimeCheck : PRE:    S278 = []                                              from Values(Sym(7), Sym(278))
      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(278))
      // Shape: V297=[1  1  1] and S297=[3]
      val x297: MDArray[Int] = {
        val result = new Array[Int](x278)
        for(i <- List.range(0, result.length))
        result(i) = x7
        internalReshape(x278::Nil, result, "values")
      }
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(275) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(275) and Sym(7))
      // RuntimeCheck : POST:   V275 = [u409  u408  u407]                              from Bubble up value for Sym(275) <- InfixOp(-: Sym(275) and Sym(7))
      // RuntimeCheck : POST:   S275 = [3]                                             from Bubble up shape for Sym(275) <- InfixOp(-: Sym(275) and Sym(7))
      // RuntimeCheck : PRE:    S275 = S7 OR S7 = []                                   from InfixOp(-: Sym(275) and Sym(7))
      // Shape: V350=[u418  u417  u416] and S350=[3]
      val x350: MDArray[Int] = {
        val result = new Array[Int](shape(x275).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x275.content()(i) -  x7
        internalReshape(shape(x275), result, "infixOpAA")
      }
      // Shape: V49=[u11] and S49=[]
      val x49: Double = internalReshape(Nil, Array(0.16666666666666666), "knownAtCompileTime")
      // Shape: V20=[u12] and S20=[]
      val x20: Double = internalReshape(Nil, Array(0.1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V20 = [u12]                                            from Bubble up value for Sym(20) <- InfixOp(*: Sym(274) and Sym(20))
      // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(*: Sym(274) and Sym(20))
      // RuntimeCheck : POST:   S274 = [u409  u408  u407]                              from Bubble up shape for Sym(274) <- InfixOp(*: Sym(274) and Sym(20))
      // RuntimeCheck : PRE:    S274 = S20 OR S20 = []                                 from InfixOp(*: Sym(274) and Sym(20))
      // Shape: S285=[u409  u408  u407]
      val x285: MDArray[Double] = {
        val result = new Array[Double](shape(x274).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x274.content()(i) *  x20
        internalReshape(shape(x274), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   S292 = [u409  u408  u407]                              from Bubble up shape for Sym(292) <- FoldArrayWith(Sym(285), fold (Sym(290), Sym(291)) => Sym(292), Sym(347))
      // RuntimeCheck : POST:   S291 = [u409  u408  u407]                              from Bubble up shape for Sym(291) <- FoldArrayWith(Sym(285), fold (Sym(290), Sym(291)) => Sym(292), Sym(347))
      // RuntimeCheck : POST:   S290 = [u409  u408  u407]                              from Bubble up shape for Sym(290) <- FoldArrayWith(Sym(285), fold (Sym(290), Sym(291)) => Sym(292), Sym(347))
      // RuntimeCheck : POST:   S285 = [u409  u408  u407]                              from Bubble up shape for Sym(285) <- FoldArrayWith(Sym(285), fold (Sym(290), Sym(291)) => Sym(292), Sym(347))
      // RuntimeCheck : POST:   S347 = [u409  u408  u407]                              from Bubble up shape for Sym(347) <- FoldArrayWith(Sym(285), fold (Sym(290), Sym(291)) => Sym(292), Sym(347))
      // RuntimeCheck : PRE:    S285 = S347                                            from FoldArrayWith(Sym(285), fold (Sym(290), Sym(291)) => Sym(292), Sym(347))
      // RuntimeCheck : PRE:    S292 = S347                                            from FoldArrayWith(Sym(285), fold (Sym(290), Sym(291)) => Sym(292), Sym(347))
      // Shape: S348=[u409  u408  u407]
      
      val x348: MDArray[Double] = {
        val opName: String = "fold"
        var result: MDArray[Double] = x285
        val foldFunction: (MDArray[Double], MDArray[Double]) => MDArray[Double] = (x290, x291) => {
          // RuntimeCheck : POST:   S291 = [u409  u408  u407]                              from Bubble up shape for Sym(291) <- InfixOp(+: Sym(290) and Sym(291))
          // RuntimeCheck : POST:   S290 = [u409  u408  u407]                              from Bubble up shape for Sym(290) <- InfixOp(+: Sym(290) and Sym(291))
          // RuntimeCheck : PRE:    S290 = S291 OR S291 = []                               from InfixOp(+: Sym(290) and Sym(291))
          // Shape: S292=[u409  u408  u407]
          val x292: MDArray[Double] = {
            val result = new Array[Double](shape(x290).content().foldLeft(1)((a,b) => a*b))
            for(i <- List.range(0, result.length))
            result(i) = x290.content()(i) +  x291.content()(i)
            internalReshape(shape(x290), result, "infixOpAA")
          }
          x292
        }
        // Shape: V4=[u2] and S4=[]
        val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
        // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
        // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
        // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
        // Shape: V5=[u261] and S5=[]
        val x5: Boolean = x4
        // RuntimeCheck : POST:   V5 = [u261]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
        // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
        // Shape: V14=[u260] and S14=[]
        val x14: Boolean = x5
        // RuntimeCheck : POST:   V14 = [u260]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
        // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
        // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
        // Shape: V15=[u259] and S15=[]
        val x15: Boolean = x14
        // Shape: V96=[3  3  3] and S96=[3]
        val x96: MDArray[Int] = internalReshape(3::Nil, Array(3, 3, 3), "knownAtCompileTime")
        // Shape: V97=[u14  u15  u16  u1 ... 37  u38  u39  u40] and S97=[27]
        val x97: MDArray[Double] = internalReshape(27::Nil, Array(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), "knownAtCompileTime")
        // RuntimeCheck : POST:   V97 = [u14  u15  u16  u17  u18  u19  u20  u21  u22  u23  u24  u25  u26  u27  u28  u29  u30  u31  u32  u33  u34  u35  u36  u37  u38  u39  u40]     from Bubble up value for Sym(97) <- Reshape(Sym(96), Sym(97))
        // RuntimeCheck : POST:   S97 = [27]                                             from Bubble up shape for Sym(97) <- Reshape(Sym(96), Sym(97))
        // RuntimeCheck : POST:   V96 = [3  3  3]                                        from Bubble up value for Sym(96) <- Reshape(Sym(96), Sym(97))
        // RuntimeCheck : POST:   S96 = [3]                                              from Bubble up shape for Sym(96) <- Reshape(Sym(96), Sym(97))
        // RuntimeCheck : PRE:    length(S96) = length([u5109])                          from Reshape(Sym(96), Sym(97))
        // RuntimeCheck : PRE:    prod(V96) = prod(S97)                                  from Reshape(Sym(96), Sym(97))
        // Shape: V98=[u258  u257  u256  ...  u234  u233  u232] and S98=[3  3  3]
        val x98: MDArray[Double] = reshape(x96, x97)
        // RuntimeCheck : POST:   V98 = [u258  u257  u256  u255  u254  u253  u252  u251  u250  u249  u248  u247  u246  u245  u244  u243  u242  u241  u240  u239  u238  u237  u236  u235  u234  u233  u232]     from Bubble up value for Sym(98) <- Shape(Sym(98))
        // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- Shape(Sym(98))
        // Shape: V102=[3  3  3] and S102=[3]
        val x102: MDArray[Int] = shape(x98)
        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- InfixOp(*: Sym(102) and Sym(12))
        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(*: Sym(102) and Sym(12))
        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- InfixOp(*: Sym(102) and Sym(12))
        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- InfixOp(*: Sym(102) and Sym(12))
        // RuntimeCheck : PRE:    S102 = S12 OR S12 = []                                 from InfixOp(*: Sym(102) and Sym(12))
        // Shape: V283=[u355  u354  u353] and S283=[3]
        val x283: MDArray[Int] = {
          val result = new Array[Int](shape(x102).content().foldLeft(1)((a,b) => a*b))
          for(i <- List.range(0, result.length))
          result(i) = x102.content()(i) *  x12
          internalReshape(shape(x102), result, "infixOpAA")
        }
        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
        // RuntimeCheck : PRE:    S102 = S7 OR S7 = []                                   from InfixOp(-: Sym(102) and Sym(7))
        // Shape: V284=[u346  u345  u344] and S284=[3]
        val x284: MDArray[Int] = {
          val result = new Array[Int](shape(x102).content().foldLeft(1)((a,b) => a*b))
          for(i <- List.range(0, result.length))
          result(i) = x102.content()(i) -  x7
          internalReshape(shape(x102), result, "infixOpAA")
        }
        // RuntimeCheck : POST:   V283 = [u355  u354  u353]                              from Bubble up value for Sym(283) <- Shape(Sym(283))
        // RuntimeCheck : POST:   S283 = [3]                                             from Bubble up shape for Sym(283) <- Shape(Sym(283))
        // Shape: V286=[3] and S286=[1]
        val x286: MDArray[Int] = shape(x283)
        // RuntimeCheck : POST:   V286 = [3]                                             from Bubble up value for Sym(286) <- Sel(Sym(9), Sym(286))
        // RuntimeCheck : POST:   S286 = [1]                                             from Bubble up shape for Sym(286) <- Sel(Sym(9), Sym(286))
        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(286))
        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(286))
        // RuntimeCheck : PRE:    length(S9) = length([u5106])                           from Sel(Sym(9), Sym(286))
        // RuntimeCheck : PRE:    S286(:length(V9)) < V9                                 from Sel(Sym(9), Sym(286))
        // Shape: V287=[3] and S287=[]
        
        // Shape: V287=[3] and S287=[]
        val x287: Int = x286.content()(flatten(shape(x286), x9, "sel"))
        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(287))
        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(287))
        // RuntimeCheck : POST:   V287 = [3]                                             from Bubble up value for Sym(287) <- Values(Sym(7), Sym(287))
        // RuntimeCheck : POST:   S287 = []                                              from Bubble up shape for Sym(287) <- Values(Sym(7), Sym(287))
        // RuntimeCheck : PRE:    S287 = []                                              from Values(Sym(7), Sym(287))
        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(287))
        // Shape: V288=[1  1  1] and S288=[3]
        val x288: MDArray[Int] = {
          val result = new Array[Int](x287)
          for(i <- List.range(0, result.length))
          result(i) = x7
          internalReshape(x287::Nil, result, "values")
        }
        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(287))
        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(287))
        // RuntimeCheck : POST:   V287 = [3]                                             from Bubble up value for Sym(287) <- Values(Sym(12), Sym(287))
        // RuntimeCheck : POST:   S287 = []                                              from Bubble up shape for Sym(287) <- Values(Sym(12), Sym(287))
        // RuntimeCheck : PRE:    S287 = []                                              from Values(Sym(12), Sym(287))
        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(287))
        // Shape: V289=[0  0  0] and S289=[3]
        val x289: MDArray[Int] = {
          val result = new Array[Int](x287)
          for(i <- List.range(0, result.length))
          result(i) = x12
          internalReshape(x287::Nil, result, "values")
        }
        // RuntimeCheck : POST:   S346 = [u409  u408  u407]                              from Bubble up shape for Sym(346) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   S293 = [3]                                             from Bubble up shape for Sym(293) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   V289 = [0  0  0]                                       from Bubble up value for Sym(289) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   S289 = [3]                                             from Bubble up shape for Sym(289) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   V288 = [1  1  1]                                       from Bubble up value for Sym(288) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   S288 = [3]                                             from Bubble up shape for Sym(288) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   V284 = [u346  u345  u344]                              from Bubble up value for Sym(284) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   S284 = [3]                                             from Bubble up shape for Sym(284) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   V283 = [u355  u354  u353]                              from Bubble up value for Sym(283) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : POST:   S283 = [3]                                             from Bubble up shape for Sym(283) <- With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : PRE:    length(S283) = length([u5014])                         from With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : PRE:    S284 = S283                                            from With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : PRE:    S288 = S283                                            from With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : PRE:    S289 = S283                                            from With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // RuntimeCheck : PRE:    V283 < V284                                            from With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        // Shape: S347=[u409  u408  u407]
        // with: With(lb=Sym(283) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(284) step=Sym(288) width=Sym(289)  Sym(293) => Sym(346))
        val lb0: Int = x283.content()(0)
        val ub0: Int = x284.content()(0)
        val step0: Int = x288.content()(0)
        val width0: Int = x289.content()(0)
        val ll0: Int = if (x15) lb0 + 1 else lb0
        val ul0: Int = if (x15) ub0 else ub0 + 1
        for (iv0 <- List.range(ll0, ul0)) {
          if ((iv0 - lb0) % step0 <= width0) {
            val lb1: Int = x283.content()(1)
            val ub1: Int = x284.content()(1)
            val step1: Int = x288.content()(1)
            val width1: Int = x289.content()(1)
            val ll1: Int = if (x15) lb1 + 1 else lb1
            val ul1: Int = if (x15) ub1 else ub1 + 1
            for (iv1 <- List.range(ll1, ul1)) {
              if ((iv1 - lb1) % step1 <= width1) {
                val lb2: Int = x283.content()(2)
                val ub2: Int = x284.content()(2)
                val step2: Int = x288.content()(2)
                val width2: Int = x289.content()(2)
                val ll2: Int = if (x15) lb2 + 1 else lb2
                val ul2: Int = if (x15) ub2 else ub2 + 1
                for (iv2 <- List.range(ll2, ul2)) {
                  if ((iv2 - lb2) % step2 <= width2) {
                    val x293: MDArray[Int] = iv0::iv1::iv2::Nil
                    val iv: MDArray[Int] = x293
                    val feval: MDArray[Double] = {
                      // RuntimeCheck : POST:   V275 = [u409  u408  u407]                              from Bubble up value for Sym(275) <- GenArrayWith(Sym(275) - Sym(345))
                      // RuntimeCheck : POST:   S275 = [3]                                             from Bubble up shape for Sym(275) <- GenArrayWith(Sym(275) - Sym(345))
                      // RuntimeCheck : POST:   V345 = [u459]                                          from Bubble up value for Sym(345) <- GenArrayWith(Sym(275) - Sym(345))
                      // RuntimeCheck : POST:   S345 = []                                              from Bubble up shape for Sym(345) <- GenArrayWith(Sym(275) - Sym(345))
                      // RuntimeCheck : PRE:    S275 = [u5137]                                         from GenArrayWith(Sym(275) - Sym(345))
                      // RuntimeCheck : PRE:    S275 = S279                                            from GenArrayWith(Sym(275) - Sym(345))
                      // RuntimeCheck : PRE:    V275(:length(V279)) < V279                             from GenArrayWith(Sym(275) - Sym(345))
                      // RuntimeCheck : PRE:    V275(length(V279):) = S345                             from GenArrayWith(Sym(275) - Sym(345))
                      // Shape: S346=[u409  u408  u407]
                      
                      val x346: MDArray[Double] = {
                        val opName: String = "genarray"
                        var result: Array[Double] = null
                        var rshape: Array[Int] = null
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(275) and Sym(7))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(275) and Sym(7))
                        // RuntimeCheck : POST:   V275 = [u409  u408  u407]                              from Bubble up value for Sym(275) <- InfixOp(-: Sym(275) and Sym(7))
                        // RuntimeCheck : POST:   S275 = [3]                                             from Bubble up shape for Sym(275) <- InfixOp(-: Sym(275) and Sym(7))
                        // RuntimeCheck : PRE:    S275 = S7 OR S7 = []                                   from InfixOp(-: Sym(275) and Sym(7))
                        // Shape: V296=[u415  u414  u413] and S296=[3]
                        val x296: MDArray[Int] = {
                          val result = new Array[Int](shape(x275).content().foldLeft(1)((a,b) => a*b))
                          for(i <- List.range(0, result.length))
                          result(i) = x275.content()(i) -  x7
                          internalReshape(shape(x275), result, "infixOpAA")
                        }
                        // RuntimeCheck : POST:   V344 = [u460]                                          from Bubble up value for Sym(344) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   S344 = []                                              from Bubble up shape for Sym(344) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   S298 = [3]                                             from Bubble up shape for Sym(298) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   V279 = [0  0  0]                                       from Bubble up value for Sym(279) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   S279 = [3]                                             from Bubble up shape for Sym(279) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   V297 = [1  1  1]                                       from Bubble up value for Sym(297) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   S297 = [3]                                             from Bubble up shape for Sym(297) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   V296 = [u415  u414  u413]                              from Bubble up value for Sym(296) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   S296 = [3]                                             from Bubble up shape for Sym(296) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   V279 = [0  0  0]                                       from Bubble up value for Sym(279) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : POST:   S279 = [3]                                             from Bubble up shape for Sym(279) <- With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : PRE:    length(S279) = length([u5138])                         from With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : PRE:    S296 = S279                                            from With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : PRE:    S297 = S279                                            from With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : PRE:    S279 = S279                                            from With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // RuntimeCheck : PRE:    V279 < V296                                            from With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        // Shape: V345=[u459] and S345=[]
                        // with: With(lb=Sym(279) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(296) step=Sym(297) width=Sym(279)  Sym(298) => Sym(344))
                        val lb0: Int = x279.content()(0)
                        val ub0: Int = x296.content()(0)
                        val step0: Int = x297.content()(0)
                        val width0: Int = x279.content()(0)
                        val ll0: Int = if (x15) lb0 + 1 else lb0
                        val ul0: Int = if (x15) ub0 else ub0 + 1
                        for (iv0 <- List.range(ll0, ul0)) {
                          if ((iv0 - lb0) % step0 <= width0) {
                            val lb1: Int = x279.content()(1)
                            val ub1: Int = x296.content()(1)
                            val step1: Int = x297.content()(1)
                            val width1: Int = x279.content()(1)
                            val ll1: Int = if (x15) lb1 + 1 else lb1
                            val ul1: Int = if (x15) ub1 else ub1 + 1
                            for (iv1 <- List.range(ll1, ul1)) {
                              if ((iv1 - lb1) % step1 <= width1) {
                                val lb2: Int = x279.content()(2)
                                val ub2: Int = x296.content()(2)
                                val step2: Int = x297.content()(2)
                                val width2: Int = x279.content()(2)
                                val ll2: Int = if (x15) lb2 + 1 else lb2
                                val ul2: Int = if (x15) ub2 else ub2 + 1
                                for (iv2 <- List.range(ll2, ul2)) {
                                  if ((iv2 - lb2) % step2 <= width2) {
                                    val x298: MDArray[Int] = iv0::iv1::iv2::Nil
                                    val iv: MDArray[Int] = x298
                                    val feval: MDArray[Double] = {
                                      // RuntimeCheck : POST:   V314 = [u323]                                          from Bubble up value for Sym(314) <- FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : POST:   S314 = []                                              from Bubble up shape for Sym(314) <- FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : POST:   S313 = []                                              from Bubble up shape for Sym(313) <- FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : POST:   S312 = []                                              from Bubble up shape for Sym(312) <- FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : POST:   V317 = [u510]                                          from Bubble up value for Sym(317) <- FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : POST:   S317 = []                                              from Bubble up shape for Sym(317) <- FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : PRE:    S4 = S317                                              from FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // RuntimeCheck : PRE:    S314 = S317                                            from FoldArrayWith(Sym(4), fold (Sym(312), Sym(313)) => Sym(314), Sym(317))
                                      // Shape: V318=[u322] and S318=[]
                                      
                                      val x318: Boolean = {
                                        val opName: String = "fold"
                                        var result: MDArray[Boolean] = x4
                                        val foldFunction: (MDArray[Boolean], MDArray[Boolean]) => MDArray[Boolean] = (x312, x313) => {
                                          // RuntimeCheck : POST:   S313 = []                                              from Bubble up shape for Sym(313) <- ScalarOperator Sym(312) || Sym(313)
                                          // RuntimeCheck : POST:   S312 = []                                              from Bubble up shape for Sym(312) <- ScalarOperator Sym(312) || Sym(313)
                                          // RuntimeCheck : PRE:    S312 = []                                              from ScalarOperator Sym(312) || Sym(313)
                                          // RuntimeCheck : PRE:    S313 = []                                              from ScalarOperator Sym(312) || Sym(313)
                                          // Shape: V314=[u323] and S314=[]
                                          val x314: Boolean = ((a: Boolean, b: Boolean) => a || b)(x312, x313)
                                          x314
                                        }
                                        // RuntimeCheck : POST:   S293 = [3]                                             from Bubble up shape for Sym(293) <- UnaryOp(-: Sym(293))
                                        // Shape: V294=[u352  u351  u350] and S294=[3]
                                        val x294: MDArray[Int] = {
                                          val result = new Array[Int](shape(x293).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = -x293.content()(i)
                                          internalReshape(shape(x293), result, "unaryOp")
                                        }
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   V294 = [u352  u351  u350]                              from Bubble up value for Sym(294) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   S294 = [3]                                             from Bubble up shape for Sym(294) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : PRE:    S294 = S7 OR S7 = []                                   from InfixOp(+: Sym(294) and Sym(7))
                                        // Shape: V295=[u349  u348  u347] and S295=[3]
                                        val x295: MDArray[Int] = {
                                          val result = new Array[Int](shape(x294).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x294.content()(i) +  x7
                                          internalReshape(shape(x294), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V295 = [u349  u348  u347]                              from Bubble up value for Sym(295) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : POST:   S295 = [3]                                             from Bubble up shape for Sym(295) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : POST:   S298 = [3]                                             from Bubble up shape for Sym(298) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : PRE:    S298 = S295 OR S295 = []                               from InfixOp(-: Sym(298) and Sym(295))
                                        // Shape: V299=[u473  u472  u471] and S299=[3]
                                        val x299: MDArray[Int] = {
                                          val result = new Array[Int](shape(x298).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x298.content()(i) -  x295.content()(i)
                                          internalReshape(shape(x298), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   S274 = [u409  u408  u407]                              from Bubble up shape for Sym(274) <- Dim(Sym(274))
                                        // Shape: V300=[3] and S300=[]
                                        val x300: Int = dim(x274)
                                        // RuntimeCheck : POST:   V300 = [3]                                             from Bubble up value for Sym(300) <- FromValue(Sym(300))
                                        // RuntimeCheck : POST:   S300 = []                                              from Bubble up shape for Sym(300) <- FromValue(Sym(300))
                                        // Shape: V301=[3] and S301=[]
                                        val x301: Int = x300
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(301))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(301))
                                        // RuntimeCheck : POST:   V301 = [3]                                             from Bubble up value for Sym(301) <- Values(Sym(12), Sym(301))
                                        // RuntimeCheck : POST:   S301 = []                                              from Bubble up shape for Sym(301) <- Values(Sym(12), Sym(301))
                                        // RuntimeCheck : PRE:    S301 = []                                              from Values(Sym(12), Sym(301))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(301))
                                        // Shape: V302=[0  0  0] and S302=[3]
                                        val x302: MDArray[Int] = {
                                          val result = new Array[Int](x301)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x301::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V302 = [0  0  0]                                       from Bubble up value for Sym(302) <- InfixOp(<: Sym(299) and Sym(302))
                                        // RuntimeCheck : POST:   S302 = [3]                                             from Bubble up shape for Sym(302) <- InfixOp(<: Sym(299) and Sym(302))
                                        // RuntimeCheck : POST:   V299 = [u473  u472  u471]                              from Bubble up value for Sym(299) <- InfixOp(<: Sym(299) and Sym(302))
                                        // RuntimeCheck : POST:   S299 = [3]                                             from Bubble up shape for Sym(299) <- InfixOp(<: Sym(299) and Sym(302))
                                        // RuntimeCheck : PRE:    S299 = S302 OR S302 = []                               from InfixOp(<: Sym(299) and Sym(302))
                                        // Shape: V303=[u470  u469  u468] and S303=[3]
                                        val x303: MDArray[Boolean] = {
                                          val result = new Array[Boolean](shape(x299).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x299.content()(i) <  x302.content()(i)
                                          internalReshape(shape(x299), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V303 = [u470  u469  u468]                              from Bubble up value for Sym(303) <- Dim(Sym(303))
                                        // RuntimeCheck : POST:   S303 = [3]                                             from Bubble up shape for Sym(303) <- Dim(Sym(303))
                                        // Shape: V304=[1] and S304=[]
                                        val x304: Int = dim(x303)
                                        // RuntimeCheck : POST:   V304 = [1]                                             from Bubble up value for Sym(304) <- FromValue(Sym(304))
                                        // RuntimeCheck : POST:   S304 = []                                              from Bubble up shape for Sym(304) <- FromValue(Sym(304))
                                        // Shape: V305=[1] and S305=[]
                                        val x305: Int = x304
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(305))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(305))
                                        // RuntimeCheck : POST:   V305 = [1]                                             from Bubble up value for Sym(305) <- Values(Sym(12), Sym(305))
                                        // RuntimeCheck : POST:   S305 = []                                              from Bubble up shape for Sym(305) <- Values(Sym(12), Sym(305))
                                        // RuntimeCheck : PRE:    S305 = []                                              from Values(Sym(12), Sym(305))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(305))
                                        // Shape: V306=[0] and S306=[1]
                                        val x306: MDArray[Int] = {
                                          val result = new Array[Int](x305)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x305::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V303 = [u470  u469  u468]                              from Bubble up value for Sym(303) <- Shape(Sym(303))
                                        // RuntimeCheck : POST:   S303 = [3]                                             from Bubble up shape for Sym(303) <- Shape(Sym(303))
                                        // Shape: V307=[3] and S307=[1]
                                        val x307: MDArray[Int] = shape(x303)
                                        // RuntimeCheck : POST:   V306 = [0]                                             from Bubble up value for Sym(306) <- Shape(Sym(306))
                                        // RuntimeCheck : POST:   S306 = [1]                                             from Bubble up shape for Sym(306) <- Shape(Sym(306))
                                        // Shape: V308=[1] and S308=[1]
                                        val x308: MDArray[Int] = shape(x306)
                                        // RuntimeCheck : POST:   V308 = [1]                                             from Bubble up value for Sym(308) <- Sel(Sym(9), Sym(308))
                                        // RuntimeCheck : POST:   S308 = [1]                                             from Bubble up shape for Sym(308) <- Sel(Sym(9), Sym(308))
                                        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(308))
                                        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(308))
                                        // RuntimeCheck : PRE:    length(S9) = length([u5162])                           from Sel(Sym(9), Sym(308))
                                        // RuntimeCheck : PRE:    S308(:length(V9)) < V9                                 from Sel(Sym(9), Sym(308))
                                        // Shape: V309=[1] and S309=[]
                                        
                                        // Shape: V309=[1] and S309=[]
                                        val x309: Int = x308.content()(flatten(shape(x308), x9, "sel"))
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(309))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(309))
                                        // RuntimeCheck : POST:   V309 = [1]                                             from Bubble up value for Sym(309) <- Values(Sym(7), Sym(309))
                                        // RuntimeCheck : POST:   S309 = []                                              from Bubble up shape for Sym(309) <- Values(Sym(7), Sym(309))
                                        // RuntimeCheck : PRE:    S309 = []                                              from Values(Sym(7), Sym(309))
                                        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(309))
                                        // Shape: V310=[1] and S310=[1]
                                        val x310: MDArray[Int] = {
                                          val result = new Array[Int](x309)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x7
                                          internalReshape(x309::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(309))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(309))
                                        // RuntimeCheck : POST:   V309 = [1]                                             from Bubble up value for Sym(309) <- Values(Sym(12), Sym(309))
                                        // RuntimeCheck : POST:   S309 = []                                              from Bubble up shape for Sym(309) <- Values(Sym(12), Sym(309))
                                        // RuntimeCheck : PRE:    S309 = []                                              from Values(Sym(12), Sym(309))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(309))
                                        // Shape: V311=[0] and S311=[1]
                                        val x311: MDArray[Int] = {
                                          val result = new Array[Int](x309)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x309::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V316 = [u511]                                          from Bubble up value for Sym(316) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   S316 = []                                              from Bubble up shape for Sym(316) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   V315 = [u516(<3)]                                      from Bubble up value for Sym(315) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   S315 = [1]                                             from Bubble up shape for Sym(315) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   V311 = [0]                                             from Bubble up value for Sym(311) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   S311 = [1]                                             from Bubble up shape for Sym(311) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   V310 = [1]                                             from Bubble up value for Sym(310) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   S310 = [1]                                             from Bubble up shape for Sym(310) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   V25 = [u280]                                           from Bubble up value for Sym(25) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   V307 = [3]                                             from Bubble up value for Sym(307) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   S307 = [1]                                             from Bubble up shape for Sym(307) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   V306 = [0]                                             from Bubble up value for Sym(306) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : POST:   S306 = [1]                                             from Bubble up shape for Sym(306) <- With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : PRE:    length(S306) = length([u5151])                         from With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : PRE:    S307 = S306                                            from With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : PRE:    S310 = S306                                            from With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : PRE:    S311 = S306                                            from With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // RuntimeCheck : PRE:    V306 < V307                                            from With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        // Shape: V317=[u510] and S317=[]
                                        // with: With(lb=Sym(306) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(307) step=Sym(310) width=Sym(311)  Sym(315) => Sym(316))
                                        val lb0: Int = x306.content()(0)
                                        val ub0: Int = x307.content()(0)
                                        val step0: Int = x310.content()(0)
                                        val width0: Int = x311.content()(0)
                                        val ll0: Int = if (x15) lb0 + 1 else lb0
                                        val ul0: Int = if (x25) ub0 else ub0 + 1
                                        for (iv0 <- List.range(ll0, ul0)) {
                                          if ((iv0 - lb0) % step0 <= width0) {
                                            val x315: MDArray[Int] = iv0::Nil
                                            val iv: MDArray[Int] = x315
                                            val feval: MDArray[Boolean] = {
                                              // RuntimeCheck : POST:   V303 = [u470  u469  u468]                              from Bubble up value for Sym(303) <- Sel(Sym(315), Sym(303))
                                              // RuntimeCheck : POST:   S303 = [3]                                             from Bubble up shape for Sym(303) <- Sel(Sym(315), Sym(303))
                                              // RuntimeCheck : POST:   V315 = [u516(<3)]                                      from Bubble up value for Sym(315) <- Sel(Sym(315), Sym(303))
                                              // RuntimeCheck : POST:   S315 = [1]                                             from Bubble up shape for Sym(315) <- Sel(Sym(315), Sym(303))
                                              // RuntimeCheck : PRE:    length(S315) = length([u5166])                         from Sel(Sym(315), Sym(303))
                                              // RuntimeCheck : PRE:    S303(:length(V315)) < V315                             from Sel(Sym(315), Sym(303))
                                              // Shape: V316=[u511] and S316=[]
                                              
                                              // Shape: V316=[u511] and S316=[]
                                              val x316: Boolean = x303.content()(flatten(shape(x303), x315, "sel"))
                                              x316
                                            }
                                            // the action of this loop:
                                            result = foldFunction(result, feval)
                                          } // if ((iv0 ...
                                        } // for (iv0 ...
                                        result
                                      }
                                      
                                      // RuntimeCheck : POST:   V318 = [u322]                                          from Bubble up value for Sym(318) <- ToValue(Sym(318))
                                      // RuntimeCheck : POST:   S318 = []                                              from Bubble up shape for Sym(318) <- ToValue(Sym(318))
                                      // RuntimeCheck : PRE:    length(S318) = length([])                              from ToValue(Sym(318))
                                      // Shape: V319=[u321] and S319=[]
                                      val x319: Boolean = x318
                                      // RuntimeCheck : POST:   V319 = [u321]                                          from Bubble up value for Sym(319) <- FromValue(Sym(319))
                                      // RuntimeCheck : POST:   S319 = []                                              from Bubble up shape for Sym(319) <- FromValue(Sym(319))
                                      // Shape: V320=[u320] and S320=[]
                                      val x320: Boolean = x319
                                      // RuntimeCheck : POST:   V333 = [u306]                                          from Bubble up value for Sym(333) <- FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : POST:   S333 = []                                              from Bubble up shape for Sym(333) <- FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : POST:   S332 = []                                              from Bubble up shape for Sym(332) <- FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : POST:   S331 = []                                              from Bubble up shape for Sym(331) <- FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : POST:   V336 = [u503]                                          from Bubble up value for Sym(336) <- FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : POST:   S336 = []                                              from Bubble up shape for Sym(336) <- FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : PRE:    S4 = S336                                              from FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // RuntimeCheck : PRE:    S333 = S336                                            from FoldArrayWith(Sym(4), fold (Sym(331), Sym(332)) => Sym(333), Sym(336))
                                      // Shape: V337=[u305] and S337=[]
                                      
                                      val x337: Boolean = {
                                        val opName: String = "fold"
                                        var result: MDArray[Boolean] = x4
                                        val foldFunction: (MDArray[Boolean], MDArray[Boolean]) => MDArray[Boolean] = (x331, x332) => {
                                          // RuntimeCheck : POST:   S332 = []                                              from Bubble up shape for Sym(332) <- ScalarOperator Sym(331) || Sym(332)
                                          // RuntimeCheck : POST:   S331 = []                                              from Bubble up shape for Sym(331) <- ScalarOperator Sym(331) || Sym(332)
                                          // RuntimeCheck : PRE:    S331 = []                                              from ScalarOperator Sym(331) || Sym(332)
                                          // RuntimeCheck : PRE:    S332 = []                                              from ScalarOperator Sym(331) || Sym(332)
                                          // Shape: V333=[u306] and S333=[]
                                          val x333: Boolean = ((a: Boolean, b: Boolean) => a || b)(x331, x332)
                                          x333
                                        }
                                        // RuntimeCheck : POST:   S293 = [3]                                             from Bubble up shape for Sym(293) <- UnaryOp(-: Sym(293))
                                        // Shape: V294=[u352  u351  u350] and S294=[3]
                                        val x294: MDArray[Int] = {
                                          val result = new Array[Int](shape(x293).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = -x293.content()(i)
                                          internalReshape(shape(x293), result, "unaryOp")
                                        }
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   V294 = [u352  u351  u350]                              from Bubble up value for Sym(294) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   S294 = [3]                                             from Bubble up shape for Sym(294) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : PRE:    S294 = S7 OR S7 = []                                   from InfixOp(+: Sym(294) and Sym(7))
                                        // Shape: V295=[u349  u348  u347] and S295=[3]
                                        val x295: MDArray[Int] = {
                                          val result = new Array[Int](shape(x294).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x294.content()(i) +  x7
                                          internalReshape(shape(x294), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V295 = [u349  u348  u347]                              from Bubble up value for Sym(295) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : POST:   S295 = [3]                                             from Bubble up shape for Sym(295) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : POST:   S298 = [3]                                             from Bubble up shape for Sym(298) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : PRE:    S298 = S295 OR S295 = []                               from InfixOp(-: Sym(298) and Sym(295))
                                        // Shape: V321=[u467  u466  u465] and S321=[3]
                                        val x321: MDArray[Int] = {
                                          val result = new Array[Int](shape(x298).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x298.content()(i) -  x295.content()(i)
                                          internalReshape(shape(x298), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V275 = [u409  u408  u407]                              from Bubble up value for Sym(275) <- InfixOp(>=: Sym(321) and Sym(275))
                                        // RuntimeCheck : POST:   S275 = [3]                                             from Bubble up shape for Sym(275) <- InfixOp(>=: Sym(321) and Sym(275))
                                        // RuntimeCheck : POST:   V321 = [u467  u466  u465]                              from Bubble up value for Sym(321) <- InfixOp(>=: Sym(321) and Sym(275))
                                        // RuntimeCheck : POST:   S321 = [3]                                             from Bubble up shape for Sym(321) <- InfixOp(>=: Sym(321) and Sym(275))
                                        // RuntimeCheck : PRE:    S321 = S275 OR S275 = []                               from InfixOp(>=: Sym(321) and Sym(275))
                                        // Shape: V322=[u464  u463  u462] and S322=[3]
                                        val x322: MDArray[Boolean] = {
                                          val result = new Array[Boolean](shape(x321).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x321.content()(i) >=  x275.content()(i)
                                          internalReshape(shape(x321), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V322 = [u464  u463  u462]                              from Bubble up value for Sym(322) <- Dim(Sym(322))
                                        // RuntimeCheck : POST:   S322 = [3]                                             from Bubble up shape for Sym(322) <- Dim(Sym(322))
                                        // Shape: V323=[1] and S323=[]
                                        val x323: Int = dim(x322)
                                        // RuntimeCheck : POST:   V323 = [1]                                             from Bubble up value for Sym(323) <- FromValue(Sym(323))
                                        // RuntimeCheck : POST:   S323 = []                                              from Bubble up shape for Sym(323) <- FromValue(Sym(323))
                                        // Shape: V324=[1] and S324=[]
                                        val x324: Int = x323
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(324))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(324))
                                        // RuntimeCheck : POST:   V324 = [1]                                             from Bubble up value for Sym(324) <- Values(Sym(12), Sym(324))
                                        // RuntimeCheck : POST:   S324 = []                                              from Bubble up shape for Sym(324) <- Values(Sym(12), Sym(324))
                                        // RuntimeCheck : PRE:    S324 = []                                              from Values(Sym(12), Sym(324))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(324))
                                        // Shape: V325=[0] and S325=[1]
                                        val x325: MDArray[Int] = {
                                          val result = new Array[Int](x324)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x324::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V322 = [u464  u463  u462]                              from Bubble up value for Sym(322) <- Shape(Sym(322))
                                        // RuntimeCheck : POST:   S322 = [3]                                             from Bubble up shape for Sym(322) <- Shape(Sym(322))
                                        // Shape: V326=[3] and S326=[1]
                                        val x326: MDArray[Int] = shape(x322)
                                        // RuntimeCheck : POST:   V325 = [0]                                             from Bubble up value for Sym(325) <- Shape(Sym(325))
                                        // RuntimeCheck : POST:   S325 = [1]                                             from Bubble up shape for Sym(325) <- Shape(Sym(325))
                                        // Shape: V327=[1] and S327=[1]
                                        val x327: MDArray[Int] = shape(x325)
                                        // RuntimeCheck : POST:   V327 = [1]                                             from Bubble up value for Sym(327) <- Sel(Sym(9), Sym(327))
                                        // RuntimeCheck : POST:   S327 = [1]                                             from Bubble up shape for Sym(327) <- Sel(Sym(9), Sym(327))
                                        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(327))
                                        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(327))
                                        // RuntimeCheck : PRE:    length(S9) = length([u5180])                           from Sel(Sym(9), Sym(327))
                                        // RuntimeCheck : PRE:    S327(:length(V9)) < V9                                 from Sel(Sym(9), Sym(327))
                                        // Shape: V328=[1] and S328=[]
                                        
                                        // Shape: V328=[1] and S328=[]
                                        val x328: Int = x327.content()(flatten(shape(x327), x9, "sel"))
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(328))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(328))
                                        // RuntimeCheck : POST:   V328 = [1]                                             from Bubble up value for Sym(328) <- Values(Sym(7), Sym(328))
                                        // RuntimeCheck : POST:   S328 = []                                              from Bubble up shape for Sym(328) <- Values(Sym(7), Sym(328))
                                        // RuntimeCheck : PRE:    S328 = []                                              from Values(Sym(7), Sym(328))
                                        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(328))
                                        // Shape: V329=[1] and S329=[1]
                                        val x329: MDArray[Int] = {
                                          val result = new Array[Int](x328)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x7
                                          internalReshape(x328::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(328))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(328))
                                        // RuntimeCheck : POST:   V328 = [1]                                             from Bubble up value for Sym(328) <- Values(Sym(12), Sym(328))
                                        // RuntimeCheck : POST:   S328 = []                                              from Bubble up shape for Sym(328) <- Values(Sym(12), Sym(328))
                                        // RuntimeCheck : PRE:    S328 = []                                              from Values(Sym(12), Sym(328))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(328))
                                        // Shape: V330=[0] and S330=[1]
                                        val x330: MDArray[Int] = {
                                          val result = new Array[Int](x328)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x328::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V335 = [u504]                                          from Bubble up value for Sym(335) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   S335 = []                                              from Bubble up shape for Sym(335) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   V334 = [u509(<3)]                                      from Bubble up value for Sym(334) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   S334 = [1]                                             from Bubble up shape for Sym(334) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   V330 = [0]                                             from Bubble up value for Sym(330) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   S330 = [1]                                             from Bubble up shape for Sym(330) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   V329 = [1]                                             from Bubble up value for Sym(329) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   S329 = [1]                                             from Bubble up shape for Sym(329) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   V25 = [u280]                                           from Bubble up value for Sym(25) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   V326 = [3]                                             from Bubble up value for Sym(326) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   S326 = [1]                                             from Bubble up shape for Sym(326) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   V325 = [0]                                             from Bubble up value for Sym(325) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : POST:   S325 = [1]                                             from Bubble up shape for Sym(325) <- With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : PRE:    length(S325) = length([u5169])                         from With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : PRE:    S326 = S325                                            from With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : PRE:    S329 = S325                                            from With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : PRE:    S330 = S325                                            from With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // RuntimeCheck : PRE:    V325 < V326                                            from With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        // Shape: V336=[u503] and S336=[]
                                        // with: With(lb=Sym(325) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(326) step=Sym(329) width=Sym(330)  Sym(334) => Sym(335))
                                        val lb0: Int = x325.content()(0)
                                        val ub0: Int = x326.content()(0)
                                        val step0: Int = x329.content()(0)
                                        val width0: Int = x330.content()(0)
                                        val ll0: Int = if (x15) lb0 + 1 else lb0
                                        val ul0: Int = if (x25) ub0 else ub0 + 1
                                        for (iv0 <- List.range(ll0, ul0)) {
                                          if ((iv0 - lb0) % step0 <= width0) {
                                            val x334: MDArray[Int] = iv0::Nil
                                            val iv: MDArray[Int] = x334
                                            val feval: MDArray[Boolean] = {
                                              // RuntimeCheck : POST:   V322 = [u464  u463  u462]                              from Bubble up value for Sym(322) <- Sel(Sym(334), Sym(322))
                                              // RuntimeCheck : POST:   S322 = [3]                                             from Bubble up shape for Sym(322) <- Sel(Sym(334), Sym(322))
                                              // RuntimeCheck : POST:   V334 = [u509(<3)]                                      from Bubble up value for Sym(334) <- Sel(Sym(334), Sym(322))
                                              // RuntimeCheck : POST:   S334 = [1]                                             from Bubble up shape for Sym(334) <- Sel(Sym(334), Sym(322))
                                              // RuntimeCheck : PRE:    length(S334) = length([u5184])                         from Sel(Sym(334), Sym(322))
                                              // RuntimeCheck : PRE:    S322(:length(V334)) < V334                             from Sel(Sym(334), Sym(322))
                                              // Shape: V335=[u504] and S335=[]
                                              
                                              // Shape: V335=[u504] and S335=[]
                                              val x335: Boolean = x322.content()(flatten(shape(x322), x334, "sel"))
                                              x335
                                            }
                                            // the action of this loop:
                                            result = foldFunction(result, feval)
                                          } // if ((iv0 ...
                                        } // for (iv0 ...
                                        result
                                      }
                                      
                                      // RuntimeCheck : POST:   V337 = [u305]                                          from Bubble up value for Sym(337) <- ToValue(Sym(337))
                                      // RuntimeCheck : POST:   S337 = []                                              from Bubble up shape for Sym(337) <- ToValue(Sym(337))
                                      // RuntimeCheck : PRE:    length(S337) = length([])                              from ToValue(Sym(337))
                                      // Shape: V338=[u304] and S338=[]
                                      val x338: Boolean = x337
                                      // RuntimeCheck : POST:   V338 = [u304]                                          from Bubble up value for Sym(338) <- FromValue(Sym(338))
                                      // RuntimeCheck : POST:   S338 = []                                              from Bubble up shape for Sym(338) <- FromValue(Sym(338))
                                      // Shape: V339=[u303] and S339=[]
                                      val x339: Boolean = x338
                                      // RuntimeCheck : POST:   V339 = [u303]                                          from Bubble up value for Sym(339) <- InfixOp(||: Sym(320) and Sym(339))
                                      // RuntimeCheck : POST:   S339 = []                                              from Bubble up shape for Sym(339) <- InfixOp(||: Sym(320) and Sym(339))
                                      // RuntimeCheck : POST:   V320 = [u320]                                          from Bubble up value for Sym(320) <- InfixOp(||: Sym(320) and Sym(339))
                                      // RuntimeCheck : POST:   S320 = []                                              from Bubble up shape for Sym(320) <- InfixOp(||: Sym(320) and Sym(339))
                                      // RuntimeCheck : PRE:    S320 = S339 OR S339 = []                               from InfixOp(||: Sym(320) and Sym(339))
                                      // Shape: V340=[u319] and S340=[]
                                      val x340: Boolean = {
                                        val result = new Array[Boolean](shape(x320).content().foldLeft(1)((a,b) => a*b))
                                        for(i <- List.range(0, result.length))
                                        result(i) = x320.content()(i) ||  x339
                                        internalReshape(shape(x320), result, "infixOpAA")
                                      }
                                      // RuntimeCheck : POST:   V340 = [u319]                                          from Bubble up value for Sym(340) <- ToValue(Sym(340))
                                      // RuntimeCheck : POST:   S340 = []                                              from Bubble up shape for Sym(340) <- ToValue(Sym(340))
                                      // RuntimeCheck : PRE:    length(S340) = length([])                              from ToValue(Sym(340))
                                      // Shape: V341=[u301] and S341=[]
                                      val x341: Boolean = x340
                                      val x344: scala.virtualization.lms.epfl.test7.original.MDArray[Double] = if (x341) {
                                        // Shape: V115=[u58] and S115=[]
                                        val x115: Double = internalReshape(Nil, Array(0.0), "knownAtCompileTime")
                                        x115
                                      } else {
                                        // RuntimeCheck : POST:   S293 = [3]                                             from Bubble up shape for Sym(293) <- UnaryOp(-: Sym(293))
                                        // Shape: V294=[u352  u351  u350] and S294=[3]
                                        val x294: MDArray[Int] = {
                                          val result = new Array[Int](shape(x293).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = -x293.content()(i)
                                          internalReshape(shape(x293), result, "unaryOp")
                                        }
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   V294 = [u352  u351  u350]                              from Bubble up value for Sym(294) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : POST:   S294 = [3]                                             from Bubble up shape for Sym(294) <- InfixOp(+: Sym(294) and Sym(7))
                                        // RuntimeCheck : PRE:    S294 = S7 OR S7 = []                                   from InfixOp(+: Sym(294) and Sym(7))
                                        // Shape: V295=[u349  u348  u347] and S295=[3]
                                        val x295: MDArray[Int] = {
                                          val result = new Array[Int](shape(x294).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x294.content()(i) +  x7
                                          internalReshape(shape(x294), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V295 = [u349  u348  u347]                              from Bubble up value for Sym(295) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : POST:   S295 = [3]                                             from Bubble up shape for Sym(295) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : POST:   S298 = [3]                                             from Bubble up shape for Sym(298) <- InfixOp(-: Sym(298) and Sym(295))
                                        // RuntimeCheck : PRE:    S298 = S295 OR S295 = []                               from InfixOp(-: Sym(298) and Sym(295))
                                        // Shape: V342=[u500(<u409)  u501(<u408)  u502(<u407)] and S342=[3]
                                        val x342: MDArray[Int] = {
                                          val result = new Array[Int](shape(x298).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x298.content()(i) -  x295.content()(i)
                                          internalReshape(shape(x298), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   S274 = [u409  u408  u407]                              from Bubble up shape for Sym(274) <- Sel(Sym(342), Sym(274))
                                        // RuntimeCheck : POST:   V342 = [u500(<u409)  u501(<u408)  u502(<u407)]         from Bubble up value for Sym(342) <- Sel(Sym(342), Sym(274))
                                        // RuntimeCheck : POST:   S342 = [3]                                             from Bubble up shape for Sym(342) <- Sel(Sym(342), Sym(274))
                                        // RuntimeCheck : PRE:    length(S342) = length([u5188])                         from Sel(Sym(342), Sym(274))
                                        // RuntimeCheck : PRE:    S274(:length(V342)) < V342                             from Sel(Sym(342), Sym(274))
                                        // Shape: V343=[u461] and S343=[]
                                        
                                        // Shape: V343=[u461] and S343=[]
                                        val x343: Double = x274.content()(flatten(shape(x274), x342, "sel"))
                                        x343
                                      }
                                      x344
                                    }
                                    // the action of this loop:
                                    if (result == null) {
                                      // create the array and shape
                                      result = new Array[Double](x275.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                      rshape = shape(feval).content()
                                    } else {
                                      // check shape -- this WILL be redundant due to runtime checks
                                      if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                                    }
                                    // copy new content
                                    val mainIndex: Int = flatten(x275 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                                    for (innerIndex <- List.range(0, rshape.length)) {
                                      result(mainIndex + innerIndex) = feval(innerIndex)
                                    }
                                  } // if ((iv0 ...
                                } // for (iv0 ...
                              } // if ((iv1 ...
                            } // for (iv1 ...
                          } // if ((iv2 ...
                        } // for (iv2 ...
                        internalReshape(x275 ::: rshape.toList, result, opName)
                      }
                      
                      x346
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
      
      // RuntimeCheck : POST:   V49 = [u11]                                            from Bubble up value for Sym(49) <- InfixOp(*: Sym(348) and Sym(49))
      // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- InfixOp(*: Sym(348) and Sym(49))
      // RuntimeCheck : POST:   S348 = [u409  u408  u407]                              from Bubble up shape for Sym(348) <- InfixOp(*: Sym(348) and Sym(49))
      // RuntimeCheck : PRE:    S348 = S49 OR S49 = []                                 from InfixOp(*: Sym(348) and Sym(49))
      // Shape: S349=[u409  u408  u407]
      val x349: MDArray[Double] = {
        val result = new Array[Double](shape(x348).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x348.content()(i) *  x49
        internalReshape(shape(x348), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V352 = [u481]                                          from Bubble up value for Sym(352) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   S352 = []                                              from Bubble up shape for Sym(352) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   V351 = [u488(<u409)  u489(<u408)  u490(<u407)]         from Bubble up value for Sym(351) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   S351 = [3]                                             from Bubble up shape for Sym(351) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   V279 = [0  0  0]                                       from Bubble up value for Sym(279) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   S279 = [3]                                             from Bubble up shape for Sym(279) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   V297 = [1  1  1]                                       from Bubble up value for Sym(297) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   S297 = [3]                                             from Bubble up shape for Sym(297) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   V25 = [u280]                                           from Bubble up value for Sym(25) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   V350 = [u418  u417  u416]                              from Bubble up value for Sym(350) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   S350 = [3]                                             from Bubble up shape for Sym(350) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   V25 = [u280]                                           from Bubble up value for Sym(25) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   V279 = [0  0  0]                                       from Bubble up value for Sym(279) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : POST:   S279 = [3]                                             from Bubble up shape for Sym(279) <- With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : PRE:    length(S279) = length([u5000])                         from With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : PRE:    S350 = S279                                            from With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : PRE:    S297 = S279                                            from With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : PRE:    S279 = S279                                            from With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // RuntimeCheck : PRE:    V279 < V350                                            from With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      // Shape: V353=[u480] and S353=[]
      // with: With(lb=Sym(279) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(350) step=Sym(297) width=Sym(279)  Sym(351) => Sym(352))
      val lb0: Int = x279.content()(0)
      val ub0: Int = x350.content()(0)
      val step0: Int = x297.content()(0)
      val width0: Int = x279.content()(0)
      val ll0: Int = if (x25) lb0 + 1 else lb0
      val ul0: Int = if (x25) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x279.content()(1)
          val ub1: Int = x350.content()(1)
          val step1: Int = x297.content()(1)
          val width1: Int = x279.content()(1)
          val ll1: Int = if (x25) lb1 + 1 else lb1
          val ul1: Int = if (x25) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x279.content()(2)
              val ub2: Int = x350.content()(2)
              val step2: Int = x297.content()(2)
              val width2: Int = x279.content()(2)
              val ll2: Int = if (x25) lb2 + 1 else lb2
              val ul2: Int = if (x25) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x351: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x351
                  val feval: MDArray[Double] = {
                    // RuntimeCheck : POST:   S349 = [u409  u408  u407]                              from Bubble up shape for Sym(349) <- Sel(Sym(351), Sym(349))
                    // RuntimeCheck : POST:   V351 = [u488(<u409)  u489(<u408)  u490(<u407)]         from Bubble up value for Sym(351) <- Sel(Sym(351), Sym(349))
                    // RuntimeCheck : POST:   S351 = [3]                                             from Bubble up shape for Sym(351) <- Sel(Sym(351), Sym(349))
                    // RuntimeCheck : PRE:    length(S351) = length([u5013])                         from Sel(Sym(351), Sym(349))
                    // RuntimeCheck : PRE:    S349(:length(V351)) < V351                             from Sel(Sym(351), Sym(349))
                    // Shape: V352=[u481] and S352=[]
                    
                    // Shape: V352=[u481] and S352=[]
                    val x352: Double = x349.content()(flatten(shape(x349), x351, "sel"))
                    x352
                  }
                  // the action of this loop:
                  if (rshape == null) {
                    rshape = shape(x274).drop(iv.content().length)
                  }
                  val mainIndex: Int = flatten(shape(x274), iv ::: zeros(dim(x274) - iv.content().length), opName)
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
      internalReshape(shape(x274) ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S274 = [u409  u408  u407]                              from Bubble up shape for Sym(274) <- Where(Sym(282), Sym(354), Sym(274))
    // RuntimeCheck : POST:   S354 = [u409  u408  u407]                              from Bubble up shape for Sym(354) <- Where(Sym(282), Sym(354), Sym(274))
    // RuntimeCheck : POST:   S282 = [u409  u408  u407]                              from Bubble up shape for Sym(282) <- Where(Sym(282), Sym(354), Sym(274))
    // RuntimeCheck : PRE:    S282 = S354                                            from Where(Sym(282), Sym(354), Sym(274))
    // RuntimeCheck : PRE:    S282 = S274                                            from Where(Sym(282), Sym(354), Sym(274))
    // Shape: S355=[u409  u408  u407]
    val x355: MDArray[Double] = {
      val result = new Array[Double](shape(x354).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = if (x282.content()(i)) x354.content()(i) else x274.content()(i)
      internalReshape(shape(x354), result, "where")
    }
    // RuntimeCheck : POST:   S282 = [u409  u408  u407]                              from Bubble up shape for Sym(282) <- UnaryOp(!: Sym(282))
    // Shape: S356=[u409  u408  u407]
    val x356: MDArray[Boolean] = {
      val result = new Array[Boolean](shape(x282).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = !x282.content()(i)
      internalReshape(shape(x282), result, "unaryOp")
    }
    // RuntimeCheck : POST:   S355 = [u409  u408  u407]                              from Bubble up shape for Sym(355) <- ModArrayWith(Sym(355) - Sym(431))
    // RuntimeCheck : POST:   V431 = [u422]                                          from Bubble up value for Sym(431) <- ModArrayWith(Sym(355) - Sym(431))
    // RuntimeCheck : POST:   S431 = []                                              from Bubble up shape for Sym(431) <- ModArrayWith(Sym(355) - Sym(431))
    // RuntimeCheck : PRE:    S373 = [u533]                                          from ModArrayWith(Sym(355) - Sym(431))
    // RuntimeCheck : PRE:    S373 = [LengthOf(S355)]                                from ModArrayWith(Sym(355) - Sym(431))
    // RuntimeCheck : PRE:    S355(:length(V373)) < V373                             from ModArrayWith(Sym(355) - Sym(431))
    // RuntimeCheck : PRE:    S355(length(V373):) = S431                             from ModArrayWith(Sym(355) - Sym(431))
    // Shape: S432=[u409  u408  u407]
    
    val x432: MDArray[Double] = {
      val opName: String = "modarray"
      var result: Array[Double] = new Array[Double](shape(x355).content().foldLeft(1)((a,b) => a*b))
      for (i <- List.range(0, result.length)) {
        result(i) = x355.content()(i)
      }
      var rshape: List[Int] = null
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : PRE:    length(S17) = length([])                               from ToValue(Sym(17))
      // Shape: V21=[u282] and S21=[]
      val x21: Boolean = x17
      // RuntimeCheck : POST:   V21 = [u282]                                           from Bubble up value for Sym(21) <- FromValue(Sym(21))
      // RuntimeCheck : POST:   S21 = []                                               from Bubble up shape for Sym(21) <- FromValue(Sym(21))
      // Shape: V24=[u281] and S24=[]
      val x24: Boolean = x21
      // RuntimeCheck : POST:   V24 = [u281]                                           from Bubble up value for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : PRE:    length(S24) = length([])                               from ToValue(Sym(24))
      // Shape: V25=[u280] and S25=[]
      val x25: Boolean = x24
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   S355 = [u409  u408  u407]                              from Bubble up shape for Sym(355) <- Shape(Sym(355))
      // Shape: V370=[u409  u408  u407] and S370=[3]
      val x370: MDArray[Int] = shape(x355)
      // RuntimeCheck : POST:   V370 = [u409  u408  u407]                              from Bubble up value for Sym(370) <- Shape(Sym(370))
      // RuntimeCheck : POST:   S370 = [3]                                             from Bubble up shape for Sym(370) <- Shape(Sym(370))
      // Shape: V371=[3] and S371=[1]
      val x371: MDArray[Int] = shape(x370)
      // RuntimeCheck : POST:   V371 = [3]                                             from Bubble up value for Sym(371) <- Sel(Sym(9), Sym(371))
      // RuntimeCheck : POST:   S371 = [1]                                             from Bubble up shape for Sym(371) <- Sel(Sym(9), Sym(371))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(371))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(371))
      // RuntimeCheck : PRE:    length(S9) = length([u2103])                           from Sel(Sym(9), Sym(371))
      // RuntimeCheck : PRE:    S371(:length(V9)) < V9                                 from Sel(Sym(9), Sym(371))
      // Shape: V372=[3] and S372=[]
      
      // Shape: V372=[3] and S372=[]
      val x372: Int = x371.content()(flatten(shape(x371), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(372))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(372))
      // RuntimeCheck : POST:   V372 = [3]                                             from Bubble up value for Sym(372) <- Values(Sym(12), Sym(372))
      // RuntimeCheck : POST:   S372 = []                                              from Bubble up shape for Sym(372) <- Values(Sym(12), Sym(372))
      // RuntimeCheck : PRE:    S372 = []                                              from Values(Sym(12), Sym(372))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(372))
      // Shape: V373=[0  0  0] and S373=[3]
      val x373: MDArray[Int] = {
        val result = new Array[Int](x372)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x372::Nil, result, "values")
      }
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(372))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(372))
      // RuntimeCheck : POST:   V372 = [3]                                             from Bubble up value for Sym(372) <- Values(Sym(7), Sym(372))
      // RuntimeCheck : POST:   S372 = []                                              from Bubble up shape for Sym(372) <- Values(Sym(7), Sym(372))
      // RuntimeCheck : PRE:    S372 = []                                              from Values(Sym(7), Sym(372))
      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(372))
      // Shape: V375=[1  1  1] and S375=[3]
      val x375: MDArray[Int] = {
        val result = new Array[Int](x372)
        for(i <- List.range(0, result.length))
        result(i) = x7
        internalReshape(x372::Nil, result, "values")
      }
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(370) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(370) and Sym(7))
      // RuntimeCheck : POST:   V370 = [u409  u408  u407]                              from Bubble up value for Sym(370) <- InfixOp(-: Sym(370) and Sym(7))
      // RuntimeCheck : POST:   S370 = [3]                                             from Bubble up shape for Sym(370) <- InfixOp(-: Sym(370) and Sym(7))
      // RuntimeCheck : PRE:    S370 = S7 OR S7 = []                                   from InfixOp(-: Sym(370) and Sym(7))
      // Shape: V428=[u400  u399  u398] and S428=[3]
      val x428: MDArray[Int] = {
        val result = new Array[Int](shape(x370).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x370.content()(i) -  x7
        internalReshape(shape(x370), result, "infixOpAA")
      }
      // Shape: V49=[u11] and S49=[]
      val x49: Double = internalReshape(Nil, Array(0.16666666666666666), "knownAtCompileTime")
      // Shape: V20=[u12] and S20=[]
      val x20: Double = internalReshape(Nil, Array(0.1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V20 = [u12]                                            from Bubble up value for Sym(20) <- InfixOp(*: Sym(274) and Sym(20))
      // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(*: Sym(274) and Sym(20))
      // RuntimeCheck : POST:   S274 = [u409  u408  u407]                              from Bubble up shape for Sym(274) <- InfixOp(*: Sym(274) and Sym(20))
      // RuntimeCheck : PRE:    S274 = S20 OR S20 = []                                 from InfixOp(*: Sym(274) and Sym(20))
      // Shape: S359=[u409  u408  u407]
      val x359: MDArray[Double] = {
        val result = new Array[Double](shape(x274).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x274.content()(i) *  x20
        internalReshape(shape(x274), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   S366 = [u409  u408  u407]                              from Bubble up shape for Sym(366) <- FoldArrayWith(Sym(359), fold (Sym(364), Sym(365)) => Sym(366), Sym(425))
      // RuntimeCheck : POST:   S365 = [u409  u408  u407]                              from Bubble up shape for Sym(365) <- FoldArrayWith(Sym(359), fold (Sym(364), Sym(365)) => Sym(366), Sym(425))
      // RuntimeCheck : POST:   S364 = [u409  u408  u407]                              from Bubble up shape for Sym(364) <- FoldArrayWith(Sym(359), fold (Sym(364), Sym(365)) => Sym(366), Sym(425))
      // RuntimeCheck : POST:   S359 = [u409  u408  u407]                              from Bubble up shape for Sym(359) <- FoldArrayWith(Sym(359), fold (Sym(364), Sym(365)) => Sym(366), Sym(425))
      // RuntimeCheck : POST:   S425 = [u409  u408  u407]                              from Bubble up shape for Sym(425) <- FoldArrayWith(Sym(359), fold (Sym(364), Sym(365)) => Sym(366), Sym(425))
      // RuntimeCheck : PRE:    S359 = S425                                            from FoldArrayWith(Sym(359), fold (Sym(364), Sym(365)) => Sym(366), Sym(425))
      // RuntimeCheck : PRE:    S366 = S425                                            from FoldArrayWith(Sym(359), fold (Sym(364), Sym(365)) => Sym(366), Sym(425))
      // Shape: S426=[u409  u408  u407]
      
      val x426: MDArray[Double] = {
        val opName: String = "fold"
        var result: MDArray[Double] = x359
        val foldFunction: (MDArray[Double], MDArray[Double]) => MDArray[Double] = (x364, x365) => {
          // RuntimeCheck : POST:   S365 = [u409  u408  u407]                              from Bubble up shape for Sym(365) <- InfixOp(+: Sym(364) and Sym(365))
          // RuntimeCheck : POST:   S364 = [u409  u408  u407]                              from Bubble up shape for Sym(364) <- InfixOp(+: Sym(364) and Sym(365))
          // RuntimeCheck : PRE:    S364 = S365 OR S365 = []                               from InfixOp(+: Sym(364) and Sym(365))
          // Shape: S366=[u409  u408  u407]
          val x366: MDArray[Double] = {
            val result = new Array[Double](shape(x364).content().foldLeft(1)((a,b) => a*b))
            for(i <- List.range(0, result.length))
            result(i) = x364.content()(i) +  x365.content()(i)
            internalReshape(shape(x364), result, "infixOpAA")
          }
          x366
        }
        // Shape: V4=[u2] and S4=[]
        val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
        // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
        // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
        // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
        // Shape: V5=[u261] and S5=[]
        val x5: Boolean = x4
        // RuntimeCheck : POST:   V5 = [u261]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
        // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
        // Shape: V14=[u260] and S14=[]
        val x14: Boolean = x5
        // RuntimeCheck : POST:   V14 = [u260]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
        // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
        // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
        // Shape: V15=[u259] and S15=[]
        val x15: Boolean = x14
        // Shape: V96=[3  3  3] and S96=[3]
        val x96: MDArray[Int] = internalReshape(3::Nil, Array(3, 3, 3), "knownAtCompileTime")
        // Shape: V97=[u14  u15  u16  u1 ... 37  u38  u39  u40] and S97=[27]
        val x97: MDArray[Double] = internalReshape(27::Nil, Array(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), "knownAtCompileTime")
        // RuntimeCheck : POST:   V97 = [u14  u15  u16  u17  u18  u19  u20  u21  u22  u23  u24  u25  u26  u27  u28  u29  u30  u31  u32  u33  u34  u35  u36  u37  u38  u39  u40]     from Bubble up value for Sym(97) <- Reshape(Sym(96), Sym(97))
        // RuntimeCheck : POST:   S97 = [27]                                             from Bubble up shape for Sym(97) <- Reshape(Sym(96), Sym(97))
        // RuntimeCheck : POST:   V96 = [3  3  3]                                        from Bubble up value for Sym(96) <- Reshape(Sym(96), Sym(97))
        // RuntimeCheck : POST:   S96 = [3]                                              from Bubble up shape for Sym(96) <- Reshape(Sym(96), Sym(97))
        // RuntimeCheck : PRE:    length(S96) = length([u5109])                          from Reshape(Sym(96), Sym(97))
        // RuntimeCheck : PRE:    prod(V96) = prod(S97)                                  from Reshape(Sym(96), Sym(97))
        // Shape: V98=[u258  u257  u256  ...  u234  u233  u232] and S98=[3  3  3]
        val x98: MDArray[Double] = reshape(x96, x97)
        // RuntimeCheck : POST:   V98 = [u258  u257  u256  u255  u254  u253  u252  u251  u250  u249  u248  u247  u246  u245  u244  u243  u242  u241  u240  u239  u238  u237  u236  u235  u234  u233  u232]     from Bubble up value for Sym(98) <- Shape(Sym(98))
        // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- Shape(Sym(98))
        // Shape: V102=[3  3  3] and S102=[3]
        val x102: MDArray[Int] = shape(x98)
        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- InfixOp(*: Sym(102) and Sym(12))
        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(*: Sym(102) and Sym(12))
        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- InfixOp(*: Sym(102) and Sym(12))
        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- InfixOp(*: Sym(102) and Sym(12))
        // RuntimeCheck : PRE:    S102 = S12 OR S12 = []                                 from InfixOp(*: Sym(102) and Sym(12))
        // Shape: V357=[u231  u230  u229] and S357=[3]
        val x357: MDArray[Int] = {
          val result = new Array[Int](shape(x102).content().foldLeft(1)((a,b) => a*b))
          for(i <- List.range(0, result.length))
          result(i) = x102.content()(i) *  x12
          internalReshape(shape(x102), result, "infixOpAA")
        }
        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
        // RuntimeCheck : PRE:    S102 = S7 OR S7 = []                                   from InfixOp(-: Sym(102) and Sym(7))
        // Shape: V358=[u228  u227  u226] and S358=[3]
        val x358: MDArray[Int] = {
          val result = new Array[Int](shape(x102).content().foldLeft(1)((a,b) => a*b))
          for(i <- List.range(0, result.length))
          result(i) = x102.content()(i) -  x7
          internalReshape(shape(x102), result, "infixOpAA")
        }
        // RuntimeCheck : POST:   V357 = [u231  u230  u229]                              from Bubble up value for Sym(357) <- Shape(Sym(357))
        // RuntimeCheck : POST:   S357 = [3]                                             from Bubble up shape for Sym(357) <- Shape(Sym(357))
        // Shape: V360=[3] and S360=[1]
        val x360: MDArray[Int] = shape(x357)
        // RuntimeCheck : POST:   V360 = [3]                                             from Bubble up value for Sym(360) <- Sel(Sym(9), Sym(360))
        // RuntimeCheck : POST:   S360 = [1]                                             from Bubble up shape for Sym(360) <- Sel(Sym(9), Sym(360))
        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(360))
        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(360))
        // RuntimeCheck : PRE:    length(S9) = length([u1452])                           from Sel(Sym(9), Sym(360))
        // RuntimeCheck : PRE:    S360(:length(V9)) < V9                                 from Sel(Sym(9), Sym(360))
        // Shape: V361=[3] and S361=[]
        
        // Shape: V361=[3] and S361=[]
        val x361: Int = x360.content()(flatten(shape(x360), x9, "sel"))
        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(361))
        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(361))
        // RuntimeCheck : POST:   V361 = [3]                                             from Bubble up value for Sym(361) <- Values(Sym(7), Sym(361))
        // RuntimeCheck : POST:   S361 = []                                              from Bubble up shape for Sym(361) <- Values(Sym(7), Sym(361))
        // RuntimeCheck : PRE:    S361 = []                                              from Values(Sym(7), Sym(361))
        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(361))
        // Shape: V362=[1  1  1] and S362=[3]
        val x362: MDArray[Int] = {
          val result = new Array[Int](x361)
          for(i <- List.range(0, result.length))
          result(i) = x7
          internalReshape(x361::Nil, result, "values")
        }
        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(361))
        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(361))
        // RuntimeCheck : POST:   V361 = [3]                                             from Bubble up value for Sym(361) <- Values(Sym(12), Sym(361))
        // RuntimeCheck : POST:   S361 = []                                              from Bubble up shape for Sym(361) <- Values(Sym(12), Sym(361))
        // RuntimeCheck : PRE:    S361 = []                                              from Values(Sym(12), Sym(361))
        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(361))
        // Shape: V363=[0  0  0] and S363=[3]
        val x363: MDArray[Int] = {
          val result = new Array[Int](x361)
          for(i <- List.range(0, result.length))
          result(i) = x12
          internalReshape(x361::Nil, result, "values")
        }
        // RuntimeCheck : POST:   S424 = [u409  u408  u407]                              from Bubble up shape for Sym(424) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   S367 = [3]                                             from Bubble up shape for Sym(367) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   V363 = [0  0  0]                                       from Bubble up value for Sym(363) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   S363 = [3]                                             from Bubble up shape for Sym(363) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   V362 = [1  1  1]                                       from Bubble up value for Sym(362) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   S362 = [3]                                             from Bubble up shape for Sym(362) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   V358 = [u228  u227  u226]                              from Bubble up value for Sym(358) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   S358 = [3]                                             from Bubble up shape for Sym(358) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   V357 = [u231  u230  u229]                              from Bubble up value for Sym(357) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : POST:   S357 = [3]                                             from Bubble up shape for Sym(357) <- With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : PRE:    length(S357) = length([u1360])                         from With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : PRE:    S358 = S357                                            from With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : PRE:    S362 = S357                                            from With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : PRE:    S363 = S357                                            from With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // RuntimeCheck : PRE:    V357 < V358                                            from With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        // Shape: S425=[u409  u408  u407]
        // with: With(lb=Sym(357) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(358) step=Sym(362) width=Sym(363)  Sym(367) => Sym(424))
        val lb0: Int = x357.content()(0)
        val ub0: Int = x358.content()(0)
        val step0: Int = x362.content()(0)
        val width0: Int = x363.content()(0)
        val ll0: Int = if (x15) lb0 + 1 else lb0
        val ul0: Int = if (x15) ub0 else ub0 + 1
        for (iv0 <- List.range(ll0, ul0)) {
          if ((iv0 - lb0) % step0 <= width0) {
            val lb1: Int = x357.content()(1)
            val ub1: Int = x358.content()(1)
            val step1: Int = x362.content()(1)
            val width1: Int = x363.content()(1)
            val ll1: Int = if (x15) lb1 + 1 else lb1
            val ul1: Int = if (x15) ub1 else ub1 + 1
            for (iv1 <- List.range(ll1, ul1)) {
              if ((iv1 - lb1) % step1 <= width1) {
                val lb2: Int = x357.content()(2)
                val ub2: Int = x358.content()(2)
                val step2: Int = x362.content()(2)
                val width2: Int = x363.content()(2)
                val ll2: Int = if (x15) lb2 + 1 else lb2
                val ul2: Int = if (x15) ub2 else ub2 + 1
                for (iv2 <- List.range(ll2, ul2)) {
                  if ((iv2 - lb2) % step2 <= width2) {
                    val x367: MDArray[Int] = iv0::iv1::iv2::Nil
                    val iv: MDArray[Int] = x367
                    val feval: MDArray[Double] = {
                      // RuntimeCheck : POST:   V370 = [u409  u408  u407]                              from Bubble up value for Sym(370) <- GenArrayWith(Sym(370) - Sym(423))
                      // RuntimeCheck : POST:   S370 = [3]                                             from Bubble up shape for Sym(370) <- GenArrayWith(Sym(370) - Sym(423))
                      // RuntimeCheck : POST:   V423 = [u410]                                          from Bubble up value for Sym(423) <- GenArrayWith(Sym(370) - Sym(423))
                      // RuntimeCheck : POST:   S423 = []                                              from Bubble up shape for Sym(423) <- GenArrayWith(Sym(370) - Sym(423))
                      // RuntimeCheck : PRE:    S370 = [u1483]                                         from GenArrayWith(Sym(370) - Sym(423))
                      // RuntimeCheck : PRE:    S370 = S373                                            from GenArrayWith(Sym(370) - Sym(423))
                      // RuntimeCheck : PRE:    V370(:length(V373)) < V373                             from GenArrayWith(Sym(370) - Sym(423))
                      // RuntimeCheck : PRE:    V370(length(V373):) = S423                             from GenArrayWith(Sym(370) - Sym(423))
                      // Shape: S424=[u409  u408  u407]
                      
                      val x424: MDArray[Double] = {
                        val opName: String = "genarray"
                        var result: Array[Double] = null
                        var rshape: Array[Int] = null
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(370) and Sym(7))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(370) and Sym(7))
                        // RuntimeCheck : POST:   V370 = [u409  u408  u407]                              from Bubble up value for Sym(370) <- InfixOp(-: Sym(370) and Sym(7))
                        // RuntimeCheck : POST:   S370 = [3]                                             from Bubble up shape for Sym(370) <- InfixOp(-: Sym(370) and Sym(7))
                        // RuntimeCheck : PRE:    S370 = S7 OR S7 = []                                   from InfixOp(-: Sym(370) and Sym(7))
                        // Shape: V374=[u397  u396  u395] and S374=[3]
                        val x374: MDArray[Int] = {
                          val result = new Array[Int](shape(x370).content().foldLeft(1)((a,b) => a*b))
                          for(i <- List.range(0, result.length))
                          result(i) = x370.content()(i) -  x7
                          internalReshape(shape(x370), result, "infixOpAA")
                        }
                        // RuntimeCheck : POST:   V422 = [u411]                                          from Bubble up value for Sym(422) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   S422 = []                                              from Bubble up shape for Sym(422) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   S376 = [3]                                             from Bubble up shape for Sym(376) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   V373 = [0  0  0]                                       from Bubble up value for Sym(373) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   S373 = [3]                                             from Bubble up shape for Sym(373) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   V375 = [1  1  1]                                       from Bubble up value for Sym(375) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   S375 = [3]                                             from Bubble up shape for Sym(375) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   V374 = [u397  u396  u395]                              from Bubble up value for Sym(374) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   S374 = [3]                                             from Bubble up shape for Sym(374) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   V373 = [0  0  0]                                       from Bubble up value for Sym(373) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : POST:   S373 = [3]                                             from Bubble up shape for Sym(373) <- With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : PRE:    length(S373) = length([u1484])                         from With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : PRE:    S374 = S373                                            from With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : PRE:    S375 = S373                                            from With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : PRE:    S373 = S373                                            from With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // RuntimeCheck : PRE:    V373 < V374                                            from With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        // Shape: V423=[u410] and S423=[]
                        // with: With(lb=Sym(373) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(374) step=Sym(375) width=Sym(373)  Sym(376) => Sym(422))
                        val lb0: Int = x373.content()(0)
                        val ub0: Int = x374.content()(0)
                        val step0: Int = x375.content()(0)
                        val width0: Int = x373.content()(0)
                        val ll0: Int = if (x15) lb0 + 1 else lb0
                        val ul0: Int = if (x15) ub0 else ub0 + 1
                        for (iv0 <- List.range(ll0, ul0)) {
                          if ((iv0 - lb0) % step0 <= width0) {
                            val lb1: Int = x373.content()(1)
                            val ub1: Int = x374.content()(1)
                            val step1: Int = x375.content()(1)
                            val width1: Int = x373.content()(1)
                            val ll1: Int = if (x15) lb1 + 1 else lb1
                            val ul1: Int = if (x15) ub1 else ub1 + 1
                            for (iv1 <- List.range(ll1, ul1)) {
                              if ((iv1 - lb1) % step1 <= width1) {
                                val lb2: Int = x373.content()(2)
                                val ub2: Int = x374.content()(2)
                                val step2: Int = x375.content()(2)
                                val width2: Int = x373.content()(2)
                                val ll2: Int = if (x15) lb2 + 1 else lb2
                                val ul2: Int = if (x15) ub2 else ub2 + 1
                                for (iv2 <- List.range(ll2, ul2)) {
                                  if ((iv2 - lb2) % step2 <= width2) {
                                    val x376: MDArray[Int] = iv0::iv1::iv2::Nil
                                    val iv: MDArray[Int] = x376
                                    val feval: MDArray[Double] = {
                                      // RuntimeCheck : POST:   V392 = [u167]                                          from Bubble up value for Sym(392) <- FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : POST:   S392 = []                                              from Bubble up shape for Sym(392) <- FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : POST:   S391 = []                                              from Bubble up shape for Sym(391) <- FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : POST:   S390 = []                                              from Bubble up shape for Sym(390) <- FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : POST:   V395 = [u446]                                          from Bubble up value for Sym(395) <- FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : POST:   S395 = []                                              from Bubble up shape for Sym(395) <- FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : PRE:    S4 = S395                                              from FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // RuntimeCheck : PRE:    S392 = S395                                            from FoldArrayWith(Sym(4), fold (Sym(390), Sym(391)) => Sym(392), Sym(395))
                                      // Shape: V396=[u263] and S396=[]
                                      
                                      val x396: Boolean = {
                                        val opName: String = "fold"
                                        var result: MDArray[Boolean] = x4
                                        val foldFunction: (MDArray[Boolean], MDArray[Boolean]) => MDArray[Boolean] = (x390, x391) => {
                                          // RuntimeCheck : POST:   S391 = []                                              from Bubble up shape for Sym(391) <- ScalarOperator Sym(390) || Sym(391)
                                          // RuntimeCheck : POST:   S390 = []                                              from Bubble up shape for Sym(390) <- ScalarOperator Sym(390) || Sym(391)
                                          // RuntimeCheck : PRE:    S390 = []                                              from ScalarOperator Sym(390) || Sym(391)
                                          // RuntimeCheck : PRE:    S391 = []                                              from ScalarOperator Sym(390) || Sym(391)
                                          // Shape: V392=[u167] and S392=[]
                                          val x392: Boolean = ((a: Boolean, b: Boolean) => a || b)(x390, x391)
                                          x392
                                        }
                                        // RuntimeCheck : POST:   S367 = [3]                                             from Bubble up shape for Sym(367) <- UnaryOp(-: Sym(367))
                                        // Shape: V368=[u225  u224  u223] and S368=[3]
                                        val x368: MDArray[Int] = {
                                          val result = new Array[Int](shape(x367).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = -x367.content()(i)
                                          internalReshape(shape(x367), result, "unaryOp")
                                        }
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   V368 = [u225  u224  u223]                              from Bubble up value for Sym(368) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   S368 = [3]                                             from Bubble up shape for Sym(368) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : PRE:    S368 = S7 OR S7 = []                                   from InfixOp(+: Sym(368) and Sym(7))
                                        // Shape: V369=[u222  u221  u220] and S369=[3]
                                        val x369: MDArray[Int] = {
                                          val result = new Array[Int](shape(x368).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x368.content()(i) +  x7
                                          internalReshape(shape(x368), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V369 = [u222  u221  u220]                              from Bubble up value for Sym(369) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : POST:   S369 = [3]                                             from Bubble up shape for Sym(369) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : POST:   S376 = [3]                                             from Bubble up shape for Sym(376) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : PRE:    S376 = S369 OR S369 = []                               from InfixOp(-: Sym(376) and Sym(369))
                                        // Shape: V377=[u394  u393  u392] and S377=[3]
                                        val x377: MDArray[Int] = {
                                          val result = new Array[Int](shape(x376).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x376.content()(i) -  x369.content()(i)
                                          internalReshape(shape(x376), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   S355 = [u409  u408  u407]                              from Bubble up shape for Sym(355) <- Dim(Sym(355))
                                        // Shape: V378=[3] and S378=[]
                                        val x378: Int = dim(x355)
                                        // RuntimeCheck : POST:   V378 = [3]                                             from Bubble up value for Sym(378) <- FromValue(Sym(378))
                                        // RuntimeCheck : POST:   S378 = []                                              from Bubble up shape for Sym(378) <- FromValue(Sym(378))
                                        // Shape: V379=[3] and S379=[]
                                        val x379: Int = x378
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(379))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(379))
                                        // RuntimeCheck : POST:   V379 = [3]                                             from Bubble up value for Sym(379) <- Values(Sym(12), Sym(379))
                                        // RuntimeCheck : POST:   S379 = []                                              from Bubble up shape for Sym(379) <- Values(Sym(12), Sym(379))
                                        // RuntimeCheck : PRE:    S379 = []                                              from Values(Sym(12), Sym(379))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(379))
                                        // Shape: V380=[0  0  0] and S380=[3]
                                        val x380: MDArray[Int] = {
                                          val result = new Array[Int](x379)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x379::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V380 = [0  0  0]                                       from Bubble up value for Sym(380) <- InfixOp(<: Sym(377) and Sym(380))
                                        // RuntimeCheck : POST:   S380 = [3]                                             from Bubble up shape for Sym(380) <- InfixOp(<: Sym(377) and Sym(380))
                                        // RuntimeCheck : POST:   V377 = [u394  u393  u392]                              from Bubble up value for Sym(377) <- InfixOp(<: Sym(377) and Sym(380))
                                        // RuntimeCheck : POST:   S377 = [3]                                             from Bubble up shape for Sym(377) <- InfixOp(<: Sym(377) and Sym(380))
                                        // RuntimeCheck : PRE:    S377 = S380 OR S380 = []                               from InfixOp(<: Sym(377) and Sym(380))
                                        // Shape: V381=[u391  u390  u389] and S381=[3]
                                        val x381: MDArray[Boolean] = {
                                          val result = new Array[Boolean](shape(x377).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x377.content()(i) <  x380.content()(i)
                                          internalReshape(shape(x377), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V381 = [u391  u390  u389]                              from Bubble up value for Sym(381) <- Dim(Sym(381))
                                        // RuntimeCheck : POST:   S381 = [3]                                             from Bubble up shape for Sym(381) <- Dim(Sym(381))
                                        // Shape: V382=[1] and S382=[]
                                        val x382: Int = dim(x381)
                                        // RuntimeCheck : POST:   V382 = [1]                                             from Bubble up value for Sym(382) <- FromValue(Sym(382))
                                        // RuntimeCheck : POST:   S382 = []                                              from Bubble up shape for Sym(382) <- FromValue(Sym(382))
                                        // Shape: V383=[1] and S383=[]
                                        val x383: Int = x382
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(383))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(383))
                                        // RuntimeCheck : POST:   V383 = [1]                                             from Bubble up value for Sym(383) <- Values(Sym(12), Sym(383))
                                        // RuntimeCheck : POST:   S383 = []                                              from Bubble up shape for Sym(383) <- Values(Sym(12), Sym(383))
                                        // RuntimeCheck : PRE:    S383 = []                                              from Values(Sym(12), Sym(383))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(383))
                                        // Shape: V384=[0] and S384=[1]
                                        val x384: MDArray[Int] = {
                                          val result = new Array[Int](x383)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x383::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V381 = [u391  u390  u389]                              from Bubble up value for Sym(381) <- Shape(Sym(381))
                                        // RuntimeCheck : POST:   S381 = [3]                                             from Bubble up shape for Sym(381) <- Shape(Sym(381))
                                        // Shape: V385=[3] and S385=[1]
                                        val x385: MDArray[Int] = shape(x381)
                                        // RuntimeCheck : POST:   V384 = [0]                                             from Bubble up value for Sym(384) <- Shape(Sym(384))
                                        // RuntimeCheck : POST:   S384 = [1]                                             from Bubble up shape for Sym(384) <- Shape(Sym(384))
                                        // Shape: V386=[1] and S386=[1]
                                        val x386: MDArray[Int] = shape(x384)
                                        // RuntimeCheck : POST:   V386 = [1]                                             from Bubble up value for Sym(386) <- Sel(Sym(9), Sym(386))
                                        // RuntimeCheck : POST:   S386 = [1]                                             from Bubble up shape for Sym(386) <- Sel(Sym(9), Sym(386))
                                        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(386))
                                        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(386))
                                        // RuntimeCheck : PRE:    length(S9) = length([u2929])                           from Sel(Sym(9), Sym(386))
                                        // RuntimeCheck : PRE:    S386(:length(V9)) < V9                                 from Sel(Sym(9), Sym(386))
                                        // Shape: V387=[1] and S387=[]
                                        
                                        // Shape: V387=[1] and S387=[]
                                        val x387: Int = x386.content()(flatten(shape(x386), x9, "sel"))
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(387))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(387))
                                        // RuntimeCheck : POST:   V387 = [1]                                             from Bubble up value for Sym(387) <- Values(Sym(7), Sym(387))
                                        // RuntimeCheck : POST:   S387 = []                                              from Bubble up shape for Sym(387) <- Values(Sym(7), Sym(387))
                                        // RuntimeCheck : PRE:    S387 = []                                              from Values(Sym(7), Sym(387))
                                        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(387))
                                        // Shape: V388=[1] and S388=[1]
                                        val x388: MDArray[Int] = {
                                          val result = new Array[Int](x387)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x7
                                          internalReshape(x387::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(387))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(387))
                                        // RuntimeCheck : POST:   V387 = [1]                                             from Bubble up value for Sym(387) <- Values(Sym(12), Sym(387))
                                        // RuntimeCheck : POST:   S387 = []                                              from Bubble up shape for Sym(387) <- Values(Sym(12), Sym(387))
                                        // RuntimeCheck : PRE:    S387 = []                                              from Values(Sym(12), Sym(387))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(387))
                                        // Shape: V389=[0] and S389=[1]
                                        val x389: MDArray[Int] = {
                                          val result = new Array[Int](x387)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x387::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V394 = [u447]                                          from Bubble up value for Sym(394) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   S394 = []                                              from Bubble up shape for Sym(394) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   V393 = [u452(<3)]                                      from Bubble up value for Sym(393) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   S393 = [1]                                             from Bubble up shape for Sym(393) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   V389 = [0]                                             from Bubble up value for Sym(389) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   S389 = [1]                                             from Bubble up shape for Sym(389) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   V388 = [1]                                             from Bubble up value for Sym(388) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   S388 = [1]                                             from Bubble up shape for Sym(388) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   V25 = [u280]                                           from Bubble up value for Sym(25) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   V385 = [3]                                             from Bubble up value for Sym(385) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   S385 = [1]                                             from Bubble up shape for Sym(385) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   V384 = [0]                                             from Bubble up value for Sym(384) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : POST:   S384 = [1]                                             from Bubble up shape for Sym(384) <- With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : PRE:    length(S384) = length([u2309])                         from With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : PRE:    S385 = S384                                            from With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : PRE:    S388 = S384                                            from With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : PRE:    S389 = S384                                            from With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // RuntimeCheck : PRE:    V384 < V385                                            from With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        // Shape: V395=[u446] and S395=[]
                                        // with: With(lb=Sym(384) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(385) step=Sym(388) width=Sym(389)  Sym(393) => Sym(394))
                                        val lb0: Int = x384.content()(0)
                                        val ub0: Int = x385.content()(0)
                                        val step0: Int = x388.content()(0)
                                        val width0: Int = x389.content()(0)
                                        val ll0: Int = if (x15) lb0 + 1 else lb0
                                        val ul0: Int = if (x25) ub0 else ub0 + 1
                                        for (iv0 <- List.range(ll0, ul0)) {
                                          if ((iv0 - lb0) % step0 <= width0) {
                                            val x393: MDArray[Int] = iv0::Nil
                                            val iv: MDArray[Int] = x393
                                            val feval: MDArray[Boolean] = {
                                              // RuntimeCheck : POST:   V381 = [u391  u390  u389]                              from Bubble up value for Sym(381) <- Sel(Sym(393), Sym(381))
                                              // RuntimeCheck : POST:   S381 = [3]                                             from Bubble up shape for Sym(381) <- Sel(Sym(393), Sym(381))
                                              // RuntimeCheck : POST:   V393 = [u452(<3)]                                      from Bubble up value for Sym(393) <- Sel(Sym(393), Sym(381))
                                              // RuntimeCheck : POST:   S393 = [1]                                             from Bubble up shape for Sym(393) <- Sel(Sym(393), Sym(381))
                                              // RuntimeCheck : PRE:    length(S393) = length([u3136])                         from Sel(Sym(393), Sym(381))
                                              // RuntimeCheck : PRE:    S381(:length(V393)) < V393                             from Sel(Sym(393), Sym(381))
                                              // Shape: V394=[u447] and S394=[]
                                              
                                              // Shape: V394=[u447] and S394=[]
                                              val x394: Boolean = x381.content()(flatten(shape(x381), x393, "sel"))
                                              x394
                                            }
                                            // the action of this loop:
                                            result = foldFunction(result, feval)
                                          } // if ((iv0 ...
                                        } // for (iv0 ...
                                        result
                                      }
                                      
                                      // RuntimeCheck : POST:   V396 = [u263]                                          from Bubble up value for Sym(396) <- ToValue(Sym(396))
                                      // RuntimeCheck : POST:   S396 = []                                              from Bubble up shape for Sym(396) <- ToValue(Sym(396))
                                      // RuntimeCheck : PRE:    length(S396) = length([])                              from ToValue(Sym(396))
                                      // Shape: V397=[u163] and S397=[]
                                      val x397: Boolean = x396
                                      // RuntimeCheck : POST:   V397 = [u163]                                          from Bubble up value for Sym(397) <- FromValue(Sym(397))
                                      // RuntimeCheck : POST:   S397 = []                                              from Bubble up shape for Sym(397) <- FromValue(Sym(397))
                                      // Shape: V398=[u162] and S398=[]
                                      val x398: Boolean = x397
                                      // RuntimeCheck : POST:   V411 = [u149]                                          from Bubble up value for Sym(411) <- FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : POST:   S411 = []                                              from Bubble up shape for Sym(411) <- FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : POST:   S410 = []                                              from Bubble up shape for Sym(410) <- FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : POST:   S409 = []                                              from Bubble up shape for Sym(409) <- FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : POST:   V414 = [u439]                                          from Bubble up value for Sym(414) <- FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : POST:   S414 = []                                              from Bubble up shape for Sym(414) <- FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : PRE:    S4 = S414                                              from FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // RuntimeCheck : PRE:    S411 = S414                                            from FoldArrayWith(Sym(4), fold (Sym(409), Sym(410)) => Sym(411), Sym(414))
                                      // Shape: V415=[u262] and S415=[]
                                      
                                      val x415: Boolean = {
                                        val opName: String = "fold"
                                        var result: MDArray[Boolean] = x4
                                        val foldFunction: (MDArray[Boolean], MDArray[Boolean]) => MDArray[Boolean] = (x409, x410) => {
                                          // RuntimeCheck : POST:   S410 = []                                              from Bubble up shape for Sym(410) <- ScalarOperator Sym(409) || Sym(410)
                                          // RuntimeCheck : POST:   S409 = []                                              from Bubble up shape for Sym(409) <- ScalarOperator Sym(409) || Sym(410)
                                          // RuntimeCheck : PRE:    S409 = []                                              from ScalarOperator Sym(409) || Sym(410)
                                          // RuntimeCheck : PRE:    S410 = []                                              from ScalarOperator Sym(409) || Sym(410)
                                          // Shape: V411=[u149] and S411=[]
                                          val x411: Boolean = ((a: Boolean, b: Boolean) => a || b)(x409, x410)
                                          x411
                                        }
                                        // RuntimeCheck : POST:   S367 = [3]                                             from Bubble up shape for Sym(367) <- UnaryOp(-: Sym(367))
                                        // Shape: V368=[u225  u224  u223] and S368=[3]
                                        val x368: MDArray[Int] = {
                                          val result = new Array[Int](shape(x367).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = -x367.content()(i)
                                          internalReshape(shape(x367), result, "unaryOp")
                                        }
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   V368 = [u225  u224  u223]                              from Bubble up value for Sym(368) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   S368 = [3]                                             from Bubble up shape for Sym(368) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : PRE:    S368 = S7 OR S7 = []                                   from InfixOp(+: Sym(368) and Sym(7))
                                        // Shape: V369=[u222  u221  u220] and S369=[3]
                                        val x369: MDArray[Int] = {
                                          val result = new Array[Int](shape(x368).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x368.content()(i) +  x7
                                          internalReshape(shape(x368), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V369 = [u222  u221  u220]                              from Bubble up value for Sym(369) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : POST:   S369 = [3]                                             from Bubble up shape for Sym(369) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : POST:   S376 = [3]                                             from Bubble up shape for Sym(376) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : PRE:    S376 = S369 OR S369 = []                               from InfixOp(-: Sym(376) and Sym(369))
                                        // Shape: V399=[u388  u387  u386] and S399=[3]
                                        val x399: MDArray[Int] = {
                                          val result = new Array[Int](shape(x376).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x376.content()(i) -  x369.content()(i)
                                          internalReshape(shape(x376), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V370 = [u409  u408  u407]                              from Bubble up value for Sym(370) <- InfixOp(>=: Sym(399) and Sym(370))
                                        // RuntimeCheck : POST:   S370 = [3]                                             from Bubble up shape for Sym(370) <- InfixOp(>=: Sym(399) and Sym(370))
                                        // RuntimeCheck : POST:   V399 = [u388  u387  u386]                              from Bubble up value for Sym(399) <- InfixOp(>=: Sym(399) and Sym(370))
                                        // RuntimeCheck : POST:   S399 = [3]                                             from Bubble up shape for Sym(399) <- InfixOp(>=: Sym(399) and Sym(370))
                                        // RuntimeCheck : PRE:    S399 = S370 OR S370 = []                               from InfixOp(>=: Sym(399) and Sym(370))
                                        // Shape: V400=[u385  u384  u383] and S400=[3]
                                        val x400: MDArray[Boolean] = {
                                          val result = new Array[Boolean](shape(x399).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x399.content()(i) >=  x370.content()(i)
                                          internalReshape(shape(x399), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V400 = [u385  u384  u383]                              from Bubble up value for Sym(400) <- Dim(Sym(400))
                                        // RuntimeCheck : POST:   S400 = [3]                                             from Bubble up shape for Sym(400) <- Dim(Sym(400))
                                        // Shape: V401=[1] and S401=[]
                                        val x401: Int = dim(x400)
                                        // RuntimeCheck : POST:   V401 = [1]                                             from Bubble up value for Sym(401) <- FromValue(Sym(401))
                                        // RuntimeCheck : POST:   S401 = []                                              from Bubble up shape for Sym(401) <- FromValue(Sym(401))
                                        // Shape: V402=[1] and S402=[]
                                        val x402: Int = x401
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(402))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(402))
                                        // RuntimeCheck : POST:   V402 = [1]                                             from Bubble up value for Sym(402) <- Values(Sym(12), Sym(402))
                                        // RuntimeCheck : POST:   S402 = []                                              from Bubble up shape for Sym(402) <- Values(Sym(12), Sym(402))
                                        // RuntimeCheck : PRE:    S402 = []                                              from Values(Sym(12), Sym(402))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(402))
                                        // Shape: V403=[0] and S403=[1]
                                        val x403: MDArray[Int] = {
                                          val result = new Array[Int](x402)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x402::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V400 = [u385  u384  u383]                              from Bubble up value for Sym(400) <- Shape(Sym(400))
                                        // RuntimeCheck : POST:   S400 = [3]                                             from Bubble up shape for Sym(400) <- Shape(Sym(400))
                                        // Shape: V404=[3] and S404=[1]
                                        val x404: MDArray[Int] = shape(x400)
                                        // RuntimeCheck : POST:   V403 = [0]                                             from Bubble up value for Sym(403) <- Shape(Sym(403))
                                        // RuntimeCheck : POST:   S403 = [1]                                             from Bubble up shape for Sym(403) <- Shape(Sym(403))
                                        // Shape: V405=[1] and S405=[1]
                                        val x405: MDArray[Int] = shape(x403)
                                        // RuntimeCheck : POST:   V405 = [1]                                             from Bubble up value for Sym(405) <- Sel(Sym(9), Sym(405))
                                        // RuntimeCheck : POST:   S405 = [1]                                             from Bubble up shape for Sym(405) <- Sel(Sym(9), Sym(405))
                                        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(405))
                                        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(405))
                                        // RuntimeCheck : PRE:    length(S9) = length([u3962])                           from Sel(Sym(9), Sym(405))
                                        // RuntimeCheck : PRE:    S405(:length(V9)) < V9                                 from Sel(Sym(9), Sym(405))
                                        // Shape: V406=[1] and S406=[]
                                        
                                        // Shape: V406=[1] and S406=[]
                                        val x406: Int = x405.content()(flatten(shape(x405), x9, "sel"))
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(406))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(406))
                                        // RuntimeCheck : POST:   V406 = [1]                                             from Bubble up value for Sym(406) <- Values(Sym(7), Sym(406))
                                        // RuntimeCheck : POST:   S406 = []                                              from Bubble up shape for Sym(406) <- Values(Sym(7), Sym(406))
                                        // RuntimeCheck : PRE:    S406 = []                                              from Values(Sym(7), Sym(406))
                                        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(406))
                                        // Shape: V407=[1] and S407=[1]
                                        val x407: MDArray[Int] = {
                                          val result = new Array[Int](x406)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x7
                                          internalReshape(x406::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(406))
                                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(406))
                                        // RuntimeCheck : POST:   V406 = [1]                                             from Bubble up value for Sym(406) <- Values(Sym(12), Sym(406))
                                        // RuntimeCheck : POST:   S406 = []                                              from Bubble up shape for Sym(406) <- Values(Sym(12), Sym(406))
                                        // RuntimeCheck : PRE:    S406 = []                                              from Values(Sym(12), Sym(406))
                                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(406))
                                        // Shape: V408=[0] and S408=[1]
                                        val x408: MDArray[Int] = {
                                          val result = new Array[Int](x406)
                                          for(i <- List.range(0, result.length))
                                          result(i) = x12
                                          internalReshape(x406::Nil, result, "values")
                                        }
                                        // RuntimeCheck : POST:   V413 = [u440]                                          from Bubble up value for Sym(413) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   S413 = []                                              from Bubble up shape for Sym(413) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   V412 = [u445(<3)]                                      from Bubble up value for Sym(412) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   S412 = [1]                                             from Bubble up shape for Sym(412) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   V408 = [0]                                             from Bubble up value for Sym(408) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   S408 = [1]                                             from Bubble up shape for Sym(408) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   V407 = [1]                                             from Bubble up value for Sym(407) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   S407 = [1]                                             from Bubble up shape for Sym(407) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   V25 = [u280]                                           from Bubble up value for Sym(25) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   V404 = [3]                                             from Bubble up value for Sym(404) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   S404 = [1]                                             from Bubble up shape for Sym(404) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   V15 = [u259]                                           from Bubble up value for Sym(15) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   V403 = [0]                                             from Bubble up value for Sym(403) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : POST:   S403 = [1]                                             from Bubble up shape for Sym(403) <- With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : PRE:    length(S403) = length([u3342])                         from With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : PRE:    S404 = S403                                            from With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : PRE:    S407 = S403                                            from With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : PRE:    S408 = S403                                            from With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // RuntimeCheck : PRE:    V403 < V404                                            from With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        // Shape: V414=[u439] and S414=[]
                                        // with: With(lb=Sym(403) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(404) step=Sym(407) width=Sym(408)  Sym(412) => Sym(413))
                                        val lb0: Int = x403.content()(0)
                                        val ub0: Int = x404.content()(0)
                                        val step0: Int = x407.content()(0)
                                        val width0: Int = x408.content()(0)
                                        val ll0: Int = if (x15) lb0 + 1 else lb0
                                        val ul0: Int = if (x25) ub0 else ub0 + 1
                                        for (iv0 <- List.range(ll0, ul0)) {
                                          if ((iv0 - lb0) % step0 <= width0) {
                                            val x412: MDArray[Int] = iv0::Nil
                                            val iv: MDArray[Int] = x412
                                            val feval: MDArray[Boolean] = {
                                              // RuntimeCheck : POST:   V400 = [u385  u384  u383]                              from Bubble up value for Sym(400) <- Sel(Sym(412), Sym(400))
                                              // RuntimeCheck : POST:   S400 = [3]                                             from Bubble up shape for Sym(400) <- Sel(Sym(412), Sym(400))
                                              // RuntimeCheck : POST:   V412 = [u445(<3)]                                      from Bubble up value for Sym(412) <- Sel(Sym(412), Sym(400))
                                              // RuntimeCheck : POST:   S412 = [1]                                             from Bubble up shape for Sym(412) <- Sel(Sym(412), Sym(400))
                                              // RuntimeCheck : PRE:    length(S412) = length([u4169])                         from Sel(Sym(412), Sym(400))
                                              // RuntimeCheck : PRE:    S400(:length(V412)) < V412                             from Sel(Sym(412), Sym(400))
                                              // Shape: V413=[u440] and S413=[]
                                              
                                              // Shape: V413=[u440] and S413=[]
                                              val x413: Boolean = x400.content()(flatten(shape(x400), x412, "sel"))
                                              x413
                                            }
                                            // the action of this loop:
                                            result = foldFunction(result, feval)
                                          } // if ((iv0 ...
                                        } // for (iv0 ...
                                        result
                                      }
                                      
                                      // RuntimeCheck : POST:   V415 = [u262]                                          from Bubble up value for Sym(415) <- ToValue(Sym(415))
                                      // RuntimeCheck : POST:   S415 = []                                              from Bubble up shape for Sym(415) <- ToValue(Sym(415))
                                      // RuntimeCheck : PRE:    length(S415) = length([])                              from ToValue(Sym(415))
                                      // Shape: V416=[u145] and S416=[]
                                      val x416: Boolean = x415
                                      // RuntimeCheck : POST:   V416 = [u145]                                          from Bubble up value for Sym(416) <- FromValue(Sym(416))
                                      // RuntimeCheck : POST:   S416 = []                                              from Bubble up shape for Sym(416) <- FromValue(Sym(416))
                                      // Shape: V417=[u144] and S417=[]
                                      val x417: Boolean = x416
                                      // RuntimeCheck : POST:   V417 = [u144]                                          from Bubble up value for Sym(417) <- InfixOp(||: Sym(398) and Sym(417))
                                      // RuntimeCheck : POST:   S417 = []                                              from Bubble up shape for Sym(417) <- InfixOp(||: Sym(398) and Sym(417))
                                      // RuntimeCheck : POST:   V398 = [u162]                                          from Bubble up value for Sym(398) <- InfixOp(||: Sym(398) and Sym(417))
                                      // RuntimeCheck : POST:   S398 = []                                              from Bubble up shape for Sym(398) <- InfixOp(||: Sym(398) and Sym(417))
                                      // RuntimeCheck : PRE:    S398 = S417 OR S417 = []                               from InfixOp(||: Sym(398) and Sym(417))
                                      // Shape: V418=[u161] and S418=[]
                                      val x418: Boolean = {
                                        val result = new Array[Boolean](shape(x398).content().foldLeft(1)((a,b) => a*b))
                                        for(i <- List.range(0, result.length))
                                        result(i) = x398.content()(i) ||  x417
                                        internalReshape(shape(x398), result, "infixOpAA")
                                      }
                                      // RuntimeCheck : POST:   V418 = [u161]                                          from Bubble up value for Sym(418) <- ToValue(Sym(418))
                                      // RuntimeCheck : POST:   S418 = []                                              from Bubble up shape for Sym(418) <- ToValue(Sym(418))
                                      // RuntimeCheck : PRE:    length(S418) = length([])                              from ToValue(Sym(418))
                                      // Shape: V419=[u142] and S419=[]
                                      val x419: Boolean = x418
                                      val x422: scala.virtualization.lms.epfl.test7.original.MDArray[Double] = if (x419) {
                                        // Shape: V115=[u58] and S115=[]
                                        val x115: Double = internalReshape(Nil, Array(0.0), "knownAtCompileTime")
                                        x115
                                      } else {
                                        // RuntimeCheck : POST:   S367 = [3]                                             from Bubble up shape for Sym(367) <- UnaryOp(-: Sym(367))
                                        // Shape: V368=[u225  u224  u223] and S368=[3]
                                        val x368: MDArray[Int] = {
                                          val result = new Array[Int](shape(x367).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = -x367.content()(i)
                                          internalReshape(shape(x367), result, "unaryOp")
                                        }
                                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   V368 = [u225  u224  u223]                              from Bubble up value for Sym(368) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : POST:   S368 = [3]                                             from Bubble up shape for Sym(368) <- InfixOp(+: Sym(368) and Sym(7))
                                        // RuntimeCheck : PRE:    S368 = S7 OR S7 = []                                   from InfixOp(+: Sym(368) and Sym(7))
                                        // Shape: V369=[u222  u221  u220] and S369=[3]
                                        val x369: MDArray[Int] = {
                                          val result = new Array[Int](shape(x368).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x368.content()(i) +  x7
                                          internalReshape(shape(x368), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   V369 = [u222  u221  u220]                              from Bubble up value for Sym(369) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : POST:   S369 = [3]                                             from Bubble up shape for Sym(369) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : POST:   S376 = [3]                                             from Bubble up shape for Sym(376) <- InfixOp(-: Sym(376) and Sym(369))
                                        // RuntimeCheck : PRE:    S376 = S369 OR S369 = []                               from InfixOp(-: Sym(376) and Sym(369))
                                        // Shape: V420=[u436(<u409)  u437(<u408)  u438(<u407)] and S420=[3]
                                        val x420: MDArray[Int] = {
                                          val result = new Array[Int](shape(x376).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x376.content()(i) -  x369.content()(i)
                                          internalReshape(shape(x376), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   S355 = [u409  u408  u407]                              from Bubble up shape for Sym(355) <- Sel(Sym(420), Sym(355))
                                        // RuntimeCheck : POST:   V420 = [u436(<u409)  u437(<u408)  u438(<u407)]         from Bubble up value for Sym(420) <- Sel(Sym(420), Sym(355))
                                        // RuntimeCheck : POST:   S420 = [3]                                             from Bubble up shape for Sym(420) <- Sel(Sym(420), Sym(355))
                                        // RuntimeCheck : PRE:    length(S420) = length([u4376])                         from Sel(Sym(420), Sym(355))
                                        // RuntimeCheck : PRE:    S355(:length(V420)) < V420                             from Sel(Sym(420), Sym(355))
                                        // Shape: V421=[u412] and S421=[]
                                        
                                        // Shape: V421=[u412] and S421=[]
                                        val x421: Double = x355.content()(flatten(shape(x355), x420, "sel"))
                                        x421
                                      }
                                      x422
                                    }
                                    // the action of this loop:
                                    if (result == null) {
                                      // create the array and shape
                                      result = new Array[Double](x370.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                      rshape = shape(feval).content()
                                    } else {
                                      // check shape -- this WILL be redundant due to runtime checks
                                      if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                                    }
                                    // copy new content
                                    val mainIndex: Int = flatten(x370 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                                    for (innerIndex <- List.range(0, rshape.length)) {
                                      result(mainIndex + innerIndex) = feval(innerIndex)
                                    }
                                  } // if ((iv0 ...
                                } // for (iv0 ...
                              } // if ((iv1 ...
                            } // for (iv1 ...
                          } // if ((iv2 ...
                        } // for (iv2 ...
                        internalReshape(x370 ::: rshape.toList, result, opName)
                      }
                      
                      x424
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
      
      // RuntimeCheck : POST:   V49 = [u11]                                            from Bubble up value for Sym(49) <- InfixOp(*: Sym(426) and Sym(49))
      // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- InfixOp(*: Sym(426) and Sym(49))
      // RuntimeCheck : POST:   S426 = [u409  u408  u407]                              from Bubble up shape for Sym(426) <- InfixOp(*: Sym(426) and Sym(49))
      // RuntimeCheck : PRE:    S426 = S49 OR S49 = []                                 from InfixOp(*: Sym(426) and Sym(49))
      // Shape: S427=[u409  u408  u407]
      val x427: MDArray[Double] = {
        val result = new Array[Double](shape(x426).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x426.content()(i) *  x49
        internalReshape(shape(x426), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V430 = [u423]                                          from Bubble up value for Sym(430) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   S430 = []                                              from Bubble up shape for Sym(430) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   V429 = [u430(<u409)  u431(<u408)  u432(<u407)]         from Bubble up value for Sym(429) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   S429 = [3]                                             from Bubble up shape for Sym(429) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   V373 = [0  0  0]                                       from Bubble up value for Sym(373) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   S373 = [3]                                             from Bubble up shape for Sym(373) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   V375 = [1  1  1]                                       from Bubble up value for Sym(375) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   S375 = [3]                                             from Bubble up shape for Sym(375) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   V25 = [u280]                                           from Bubble up value for Sym(25) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   V428 = [u400  u399  u398]                              from Bubble up value for Sym(428) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   S428 = [3]                                             from Bubble up shape for Sym(428) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   V25 = [u280]                                           from Bubble up value for Sym(25) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   V373 = [0  0  0]                                       from Bubble up value for Sym(373) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : POST:   S373 = [3]                                             from Bubble up shape for Sym(373) <- With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : PRE:    length(S373) = length([u534])                          from With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : PRE:    S428 = S373                                            from With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : PRE:    S375 = S373                                            from With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : PRE:    S373 = S373                                            from With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // RuntimeCheck : PRE:    V373 < V428                                            from With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      // Shape: V431=[u422] and S431=[]
      // with: With(lb=Sym(373) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(428) step=Sym(375) width=Sym(373)  Sym(429) => Sym(430))
      val lb0: Int = x373.content()(0)
      val ub0: Int = x428.content()(0)
      val step0: Int = x375.content()(0)
      val width0: Int = x373.content()(0)
      val ll0: Int = if (x25) lb0 + 1 else lb0
      val ul0: Int = if (x25) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x373.content()(1)
          val ub1: Int = x428.content()(1)
          val step1: Int = x375.content()(1)
          val width1: Int = x373.content()(1)
          val ll1: Int = if (x25) lb1 + 1 else lb1
          val ul1: Int = if (x25) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x373.content()(2)
              val ub2: Int = x428.content()(2)
              val step2: Int = x375.content()(2)
              val width2: Int = x373.content()(2)
              val ll2: Int = if (x25) lb2 + 1 else lb2
              val ul2: Int = if (x25) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x429: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x429
                  val feval: MDArray[Double] = {
                    // RuntimeCheck : POST:   S427 = [u409  u408  u407]                              from Bubble up shape for Sym(427) <- Sel(Sym(429), Sym(427))
                    // RuntimeCheck : POST:   V429 = [u430(<u409)  u431(<u408)  u432(<u407)]         from Bubble up value for Sym(429) <- Sel(Sym(429), Sym(427))
                    // RuntimeCheck : POST:   S429 = [3]                                             from Bubble up shape for Sym(429) <- Sel(Sym(429), Sym(427))
                    // RuntimeCheck : PRE:    length(S429) = length([u1359])                         from Sel(Sym(429), Sym(427))
                    // RuntimeCheck : PRE:    S427(:length(V429)) < V429                             from Sel(Sym(429), Sym(427))
                    // Shape: V430=[u423] and S430=[]
                    
                    // Shape: V430=[u423] and S430=[]
                    val x430: Double = x427.content()(flatten(shape(x427), x429, "sel"))
                    x430
                  }
                  // the action of this loop:
                  if (rshape == null) {
                    rshape = shape(x355).drop(iv.content().length)
                  }
                  val mainIndex: Int = flatten(shape(x355), iv ::: zeros(dim(x355) - iv.content().length), opName)
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
      internalReshape(shape(x355) ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S355 = [u409  u408  u407]                              from Bubble up shape for Sym(355) <- Where(Sym(356), Sym(432), Sym(355))
    // RuntimeCheck : POST:   S432 = [u409  u408  u407]                              from Bubble up shape for Sym(432) <- Where(Sym(356), Sym(432), Sym(355))
    // RuntimeCheck : POST:   S356 = [u409  u408  u407]                              from Bubble up shape for Sym(356) <- Where(Sym(356), Sym(432), Sym(355))
    // RuntimeCheck : PRE:    S356 = S432                                            from Where(Sym(356), Sym(432), Sym(355))
    // RuntimeCheck : PRE:    S356 = S355                                            from Where(Sym(356), Sym(432), Sym(355))
    // Shape: S433=[u409  u408  u407]
    val x433: MDArray[Double] = {
      val result = new Array[Double](shape(x432).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = if (x356.content()(i)) x432.content()(i) else x355.content()(i)
      internalReshape(shape(x432), result, "where")
    }
    x433
  }
}
/*****************************************
  End of Generated Code                  
*******************************************/
