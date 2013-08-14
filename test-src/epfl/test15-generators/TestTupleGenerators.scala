package scala.virtualization.lms
package epfl
package test15

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream
import scala.reflect.SourceContext
import dbtoptimizer.lifters._
import org.dbtoaster.dbtoasterlib.K3Collection._

trait TupledGeneratorProg extends K3PersistentCollectionOps with NumericOps
  with OrderingOps with PrimitiveOps with Equal
  with Structs with MiscOps with ArrayOps {
  def test1(MST_mASKS2_E1_1_L2_1 : Rep[K3PersistentCollection[(Double), Double]], MST_mASKS2_E1_1 : Rep[K3PersistentCollection[Tuple2[Long,Double], Double]]) = {
    val var_BIDS_PRICE: Rep[Double] = unit(12.2)
    val _var___prod_ret__30:Rep[Double] = MST_mASKS2_E1_1_L2_1.map((y:Rep[Tuple2[(Double),Double]]) => {
      val v: Rep[Tuple2[Double,Double]] = ({
        (x:Rep[Tuple2[(Double), Double]]) => {
          val var_B2_PRICE:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

          val var___map_ret__18:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

          ((var_B2_PRICE),({
            val an331 = (var___map_ret__18);

            val bn331 = ((if({
              val ac93 = (var_BIDS_PRICE);

              val bc93 = (var_B2_PRICE);

              ac93<bc93
            }
            ) unit(1.0) else unit(0.0)));

            an331*bn331
          }
          )) 
        }
      }
      )(y);

      make_tuple2((v._1), v._2) 
    }
    ).fold(unit(0.0), {
      (x:Rep[Tuple2[((Double)), Double]]) => {
        val var_B2_PRICE:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

        val var___prod_ret__30:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

        (var___accv_9:Rep[Double]) => {                                                            {
            val an330 = (var___accv_9);

            val bn330 = (var___prod_ret__30);

            an330+bn330
          }
        }
      }
    }
    );
  }
  def test2(var_B_BROKER_ID: Rep[Long], MST_mASKS2_E1_1 : Rep[K3PersistentCollection[Tuple2[Long,Double], Double]]) = {
    //val var_B_BROKER_ID: Rep[Long] = unit(44L)
    val v = ({
    (x:Rep[Tuple2[(Double), Long]]) => {
      val var___sql_inline_agg_1:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

      val var___lift_ret__16:Rep[Long] = x._2 /* Rep[Long] => Rep[Long] */;

      MST_mASKS2_E1_1.slice((var_B_BROKER_ID), List(unit(0))).map((y:Rep[Tuple2[Tuple2[Long,Double],Double]]) => {
        val v: Rep[Tuple2[Double,Double]] = ({
          (x:Rep[Tuple2[Tuple2[Long,Double], Double]]) => {
            val var_B_BROKER_ID:Rep[Long] = x._1._1 /* Rep[Long] => Rep[Long] */;

            val var_B0_PRICE:Rep[Double] = x._1._2 /* Rep[Double] => Rep[Double] */;

            val var___map_ret__23:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

            ((var_B0_PRICE),(var___map_ret__23)) 
          }
        }
        )(y);

        make_tuple2((v._1), v._2) 
      }
      ).map((y:Rep[Tuple2[(Double),Double]]) => {
        val v: Rep[Tuple3[Double,Double,Double]] = ({
          (x:Rep[Tuple2[(Double), Double]]) => {
            val var_B0_PRICE:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

            val var___map_ret__23:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

            ((var___sql_inline_agg_1),(var_B0_PRICE),({
              val an318 = ((var___lift_ret__16).asInstanceOf[Rep[Double]]);

              val bn318 = (var___map_ret__23);

              an318*bn318
            }
            )) 
          }
        }
        )(make_tuple2(unit(0.13),unit(75.32)));

        make_tuple2(make_tuple2(v._1,v._2), v._3) 
      }
      ) 
    }
  }
  )(make_tuple2(unit(3.14),unit(200L))).fold(unit(0.0), {
    (x:Rep[Tuple2[(Double, Double), Double]]) => {
      val var_B2_PRICE:Rep[Double] = x._1._1 /* Rep[Double] => Rep[Double] */;

      val var___prod_ret__30:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

      (var___accv_9:Rep[Double]) => {                                                            {
          val an330 = (var___accv_9);

          val bn330 = (var___prod_ret__30);

          an330+bn330
        }
      }
    }
  }
  );

  v 
  }
}

class TestTupledGeneratorOps extends FileDiffSuite {

  val prefix = "test-out/epfl/test15-"

  def testgenerator1 = {
    withOutFile(prefix+"tupled-generator-simple"){
       new TupledGeneratorProg with GeneratorOpsExp with NumericOpsExp
        with OrderingOpsExp with PrimitiveOpsExp with EqualExp
        with StructExp with StructExpOptCommon with ArrayOpsExp
        with MiscOpsExp with ScalaCompile with K3PersistentCollectionExp
        with ExtendedExpressions { self =>

        val printWriter = new java.io.PrintWriter(System.out)

        //test1: first "loop"
        val codegen = new ScalaGenGeneratorOps with ScalaGenNumericOps
          with ScalaGenOrderingOps with ScalaGenPrimitiveOps with ScalaGenEqual
          with ScalaGenArrayOps with ScalaGenStruct with ScalaGenMiscOps
          with ScalaGenK3PersistentCollection with ScalaConciseCodegen{ val IR: self.type = self }

        codegen.emitSource2(test1 _ , "test1", printWriter)
        codegen.emitDataStructures(printWriter)
        //val testc1 = compile2s(test1, source)
        //scala.Console.println(testc1(1,11))

        //test2: a map
        codegen.emitSource2(test2 _ , "test2", printWriter)
        codegen.emitDataStructures(printWriter)
        //val testc2 = compile2(test2)
        //scala.Console.println(testc2(1,11))


      }
    }
    assertFileEqualsCheck(prefix+"tupled-generator-simple")
  }
}