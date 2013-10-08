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

trait TupledGeneratorFlattenProg extends K3PersistentCollectionOps with NumericOps
  with OrderingOps with PrimitiveOps with Equal
  with Structs with MiscOps with ArrayOps with ListContainerOps{
  def test1(var_BIDS_T: Rep[Double], var_BIDS_ID: Rep[Long], var_BIDS_BROKER_ID: Rep[Long], var_BIDS_VOLUME: Rep[Double], var_BIDS_PRICE: Rep[Double], AXFINDER_orig : Rep[K3PersistentCollection[(Long), Double]], AXFINDER_mASKS1_orig : Rep[K3PersistentCollection[Tuple2[Long,Double], Double]], AXFINDER_mASKS2_orig : Rep[K3PersistentCollection[Tuple2[Long,Double], Long]], AXFINDER_mBIDS1_orig : Rep[K3PersistentCollection[Tuple2[Long,Double], Long]], AXFINDER_mBIDS3_orig : Rep[K3PersistentCollection[Tuple2[Long,Double], Double]]) = {
    val AXFINDER = AXFINDER_orig.mutable;

    val AXFINDER_mASKS1 = AXFINDER_mASKS1_orig.mutable;

    val AXFINDER_mASKS2 = AXFINDER_mASKS2_orig.mutable;

    val AXFINDER_mBIDS1 = AXFINDER_mBIDS1_orig.mutable;

    val AXFINDER_mBIDS3 = AXFINDER_mBIDS3_orig.mutable;
    (AXFINDER_mBIDS1.slice((var_BIDS_BROKER_ID), NewList(unit(0))).map((y:Rep[Tuple2[Tuple2[Long,Double],Long]]) => {
      val v: Rep[Tuple2[Double,Long]] = ({
        (x:Rep[Tuple2[Tuple2[Long,Double], Long]]) => (x._1._2,x._2)
      }
      )(y);

      make_tuple2((v._1), v._2) 
    }
    ).map((y:Rep[Tuple2[(Double),Long]]) => {
      val v = ({
        (x:Rep[Tuple2[(Double), Long]]) => {
          val var_A_PRICE:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

          val var___map_ret__1:Rep[Long] = x._2 /* Rep[Long] => Rep[Long] */;
          {
            val v = {

              val var___sum_ret__1:Rep[Long] = ((if(unit(1000.0) < (var_A_PRICE + unit(-1L).asInstanceOf[Rep[Double]] * var_BIDS_PRICE)) unit(1L) else unit(0L))) +
                                               ((if(unit(1000.0) < (var_BIDS_PRICE + unit(-1L).asInstanceOf[Rep[Double]] * var_A_PRICE)) unit(1L) else unit(0L)));

              ((var___sum_ret__1),(unit(1L)))
            };

            newSingletonK3IntermediateCollection[(Long), Long](Tuple2((v._1),v._2)) 
          }
          .map((y:Rep[Tuple2[(Long),Long]]) => {
            val v: Rep[Tuple3[Double,Long,Long]] = ({
              (x:Rep[Tuple2[(Long), Long]]) => {
                (var_A_PRICE,x._1,var___map_ret__1 * x._2) 
              }
            }
            )(y);

            make_tuple2(make_tuple2(v._1,v._2), v._3) 
          }
          ) 
        }
      }
      )(y);

      v 
    }
    )).flatten[Tuple2[Double,Long], Long].toList
  }
}

class TestTupledGeneratorFlatten extends FileDiffSuite {

  val prefix = "test-out/epfl/test15-"

  def testgenerator1 = {
    withOutFile(prefix+"tupled-generator-flatten"){
       new TupledGeneratorFlattenProg with GeneratorOpsExp with NumericOpsExp
        with OrderingOpsExp with PrimitiveOpsExp with EqualExp
        with StructExp with StructExpOptCommon with ArrayOpsExp
        with MiscOpsExp with ScalaCompile with K3PersistentCollectionExp
        with ListContainerExp with ExtendedExpressions { self =>

        val printWriter = new java.io.PrintWriter(System.out)

        //test1: first "loop"
        val codegen = new ScalaGenGeneratorOps with ScalaGenNumericOps
          with ScalaGenOrderingOps with ScalaGenPrimitiveOps with ScalaGenEqual
          with ScalaGenArrayOps with ScalaGenStruct with ScalaGenMiscOps
          with ScalaGenK3PersistentCollection with ScalaGenListContainer with ScalaConciseCodegen{ val IR: self.type = self }

        codegen.emitSource10(test1 _ , "test1", printWriter)
        codegen.emitDataStructures(printWriter)
        //val testc1 = compile2s(test1, source)
        //scala.Console.println(testc1(1,11))


      }
    }
    assertFileEqualsCheck(prefix+"tupled-generator-flatten")
  }
}
