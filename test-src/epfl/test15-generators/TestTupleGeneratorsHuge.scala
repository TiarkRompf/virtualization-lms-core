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

trait TupledGeneratorHugeProg extends K3PersistentCollectionOps with ListContainerOps with SimpleValOps with NumericOps
  with OrderingOps with PrimitiveOps with Equal
  with Structs with MiscOps with ArrayOps {

  def VWAPonInsertBIDS(var_BIDS_T: Rep[Double], var_BIDS_ID: Rep[Long], var_BIDS_BROKER_ID: Rep[Long], var_BIDS_VOLUME: Rep[Double], var_BIDS_PRICE: Rep[Double], VWAP_orig : Rep[SimpleVal[Double]], VWAP_mBIDS1_orig : Rep[K3PersistentCollection[(Double), Double]], VWAP_mBIDS1_L1_1_orig : Rep[SimpleVal[Double]], VWAP_mBIDS1_L2_1_orig : Rep[K3PersistentCollection[(Double), Double]]) {
    val VWAP = VWAP_orig.mutable;

    val VWAP_mBIDS1 = VWAP_mBIDS1_orig.mutable;

    val VWAP_mBIDS1_L1_1 = VWAP_mBIDS1_L1_1_orig.mutable;

    val VWAP_mBIDS1_L2_1 = VWAP_mBIDS1_L2_1_orig.mutable;
    {
      val _var_existing_out_tier:Rep[K3PersistentCollection[(Double), Double]] = VWAP_mBIDS1;

      val var_existing_out_tier:Rep[K3PersistentCollection[(Double), Double]] = _var_existing_out_tier /* Rep[K3PersistentCollection[(Double), Double]] => Rep[K3PersistentCollection[(Double), Double]] */;
      {
        val _var___prod_ret__1:Rep[Double] = {
          val an0 = (var_BIDS_PRICE);

          val bn0 = (var_BIDS_VOLUME);

          an0 * bn0
        };

        val var___prod_ret__1:Rep[Double] = _var___prod_ret__1 /* Rep[Double] => Rep[Double] */;
        {
          val nv = {
            val an1 = (if((var_existing_out_tier).contains((var_BIDS_PRICE))) {
              (var_existing_out_tier).lookup((var_BIDS_PRICE)) 
            }
            else {
              unit(0.0) 
            }
            );

            val bn1 = (var___prod_ret__1);

            an1 + bn1
          };

          VWAP_mBIDS1.updateValue((var_BIDS_PRICE), nv)
        }
      }
    };
    {
      val _var_existing_out_tier:Rep[Double] = VWAP_mBIDS1_L1_1.get();

      val var_existing_out_tier:Rep[Double] = _var_existing_out_tier /* Rep[Double] => Rep[Double] */;
      {
        val _var___val_ret__3:Rep[Double] = var_BIDS_VOLUME;

        val var___val_ret__3:Rep[Double] = _var___val_ret__3 /* Rep[Double] => Rep[Double] */;

        VWAP_mBIDS1_L1_1.update({
          val an2 = (var_existing_out_tier);

          val bn2 = (var___val_ret__3);

          an2 + bn2
        }
        )
      }
    };
    {
      val _var_existing_out_tier:Rep[K3PersistentCollection[(Double), Double]] = VWAP_mBIDS1_L2_1;

      val var_existing_out_tier:Rep[K3PersistentCollection[(Double), Double]] = _var_existing_out_tier /* Rep[K3PersistentCollection[(Double), Double]] => Rep[K3PersistentCollection[(Double), Double]] */;
      {
        val _var___val_ret__4:Rep[Double] = var_BIDS_VOLUME;

        val var___val_ret__4:Rep[Double] = _var___val_ret__4 /* Rep[Double] => Rep[Double] */;
        {
          val nv = {
            val an3 = (if((var_existing_out_tier).contains((var_BIDS_PRICE))) {
              (var_existing_out_tier).lookup((var_BIDS_PRICE)) 
            }
            else {
              unit(0.0) 
            }
            );

            val bn3 = (var___val_ret__4);

            an3 + bn3
          };

          VWAP_mBIDS1_L2_1.updateValue((var_BIDS_PRICE), nv)
        }
      }
    };
    {
      val _var_existing_out_tier:Rep[Double] = VWAP.get();

      val var_existing_out_tier:Rep[Double] = _var_existing_out_tier /* Rep[Double] => Rep[Double] */;
      {
        val _var___prod_ret__6:Rep[Double] = (({
          val v = {
            val _var___prod_ret__2:Rep[Double] = {
              val an5 = (VWAP_mBIDS1_L1_1.get());

              val bn5 = (unit(0.250));

              an5 * bn5
            };

            val var___prod_ret__2:Rep[Double] = _var___prod_ret__2 /* Rep[Double] => Rep[Double] */;

            ((var___prod_ret__2),(unit(1L)))
          };

          newSingletonK3IntermediateCollection[(Double), Long](Tuple2((v._1),v._2)) 
        }
        .map((y:Rep[Tuple2[(Double),Long]]) => {
          val v = ({
            (x:Rep[Tuple2[(Double), Long]]) => {
              val var___sql_inline_agg_1:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

              val var___lift_ret__1:Rep[Long] = x._2 /* Rep[Long] => Rep[Long] */;

              VWAP_mBIDS1.map((y:Rep[Tuple2[(Double),Double]]) => {
                val v: Rep[Tuple3[Double,Double,Double]] = ({
                  (x:Rep[Tuple2[(Double), Double]]) => {
                    val var_B1_PRICE:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

                    val var___map_ret__2:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

                    ((var___sql_inline_agg_1),(var_B1_PRICE),({
                      val an6 = ((var___lift_ret__1).asInstanceOf[Rep[Double]]);

                      val bn6 = (var___map_ret__2);

                      an6 * bn6
                    }
                    )) 
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
        )).flatten[Tuple2[Double,Double], Double].map((y:Rep[Tuple2[Tuple2[Double,Double],Double]]) => {
          val v = ({
            (x:Rep[Tuple2[Tuple2[Double,Double], Double]]) => {
              val var___sql_inline_agg_1:Rep[Double] = x._1._1 /* Rep[Double] => Rep[Double] */;

              val var_B1_PRICE:Rep[Double] = x._1._2 /* Rep[Double] => Rep[Double] */;

              val var___prod_ret__4:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;
              {
                val v = {
                  val _var___prod_ret__3:Rep[Double] = VWAP_mBIDS1_L2_1.map((y:Rep[Tuple2[(Double),Double]]) => {
                    val v: Rep[Tuple2[Double,Double]] = ({
                      (x:Rep[Tuple2[(Double), Double]]) => {
                        val var_B2_PRICE:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

                        val var___map_ret__3:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

                        ((var_B2_PRICE),({
                          val an8 = (var___map_ret__3);

                          val bn8 = ((if({
                            val ac0 = (var_B1_PRICE);

                            val bc0 = (var_B2_PRICE);

                            ac0 < bc0
                          }
                          ) unit(1.0) else unit(0.0)));

                          an8 * bn8
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

                      val var___prod_ret__3:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

                      (var___accv_1:Rep[Double]) => {                            {
                          val an7 = (var___accv_1);

                          val bn7 = (var___prod_ret__3);

                          an7 + bn7
                        }
                      }
                    }
                  }
                  );

                  val var___prod_ret__3:Rep[Double] = _var___prod_ret__3 /* Rep[Double] => Rep[Double] */;

                  ((var___prod_ret__3),(unit(1L)))
                };

                newSingletonK3IntermediateCollection[(Double), Long](Tuple2((v._1),v._2)) 
              }
              .map((y:Rep[Tuple2[(Double),Long]]) => {
                val v: Rep[Tuple4[Double,Double,Double,Double]] = ({
                  (x:Rep[Tuple2[(Double), Long]]) => {
                    val var___sql_inline_agg_2:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

                    val var___lift_ret__2:Rep[Long] = x._2 /* Rep[Long] => Rep[Long] */;

                    ((var___sql_inline_agg_1),(var_B1_PRICE),(var___sql_inline_agg_2),({
                      val an9 = (var___prod_ret__4);

                      val bn9 = ((var___lift_ret__2).asInstanceOf[Rep[Double]]);

                      an9 * bn9
                    }
                    )) 
                  }
                }
                )(y);

                make_tuple2(make_tuple3(v._1,v._2,v._3), v._4) 
              }
              ) 
            }
          }
          )(y);

          v 
        }
        )).flatten[Tuple3[Double,Double,Double], Double].map((y:Rep[Tuple2[Tuple3[Double,Double,Double],Double]]) => {
          val v: Rep[Tuple4[Double,Double,Double,Double]] = ({
            (x:Rep[Tuple2[Tuple3[Double,Double,Double], Double]]) => {
              val var___sql_inline_agg_1:Rep[Double] = x._1._1 /* Rep[Double] => Rep[Double] */;

              val var_B1_PRICE:Rep[Double] = x._1._2 /* Rep[Double] => Rep[Double] */;

              val var___sql_inline_agg_2:Rep[Double] = x._1._3 /* Rep[Double] => Rep[Double] */;

              val var___prod_ret__5:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

              ((var___sql_inline_agg_1),(var_B1_PRICE),(var___sql_inline_agg_2),({
                val an10 = (var___prod_ret__5);

                val bn10 = ((if({
                  val ac1 = (var___sql_inline_agg_2);

                  val bc1 = (var___sql_inline_agg_1);

                  ac1 < bc1
                }
                ) unit(1.0) else unit(0.0)));

                an10 * bn10
              }
              )) 
            }
          }
          )(y);

          make_tuple2(make_tuple3(v._1,v._2,v._3), v._4) 
        }
        ).fold(unit(0.0), {
          (x:Rep[Tuple2[(Tuple3[Double,Double,Double]), Double]]) => {
            val var___sql_inline_agg_1:Rep[Double] = x._1._1 /* Rep[Double] => Rep[Double] */;

            val var_B1_PRICE:Rep[Double] = x._1._2 /* Rep[Double] => Rep[Double] */;

            val var___sql_inline_agg_2:Rep[Double] = x._1._3 /* Rep[Double] => Rep[Double] */;

            val var___prod_ret__6:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

            (var___accv_2:Rep[Double]) => {                  {
                val an4 = (var___accv_2);

                val bn4 = (var___prod_ret__6);

                an4 + bn4
              }
            }
          }
        }
        );

        val var___prod_ret__6:Rep[Double] = _var___prod_ret__6 /* Rep[Double] => Rep[Double] */;

        VWAP.update({
          val an11 = (unit(0.0));

          val bn11 = (var___prod_ret__6);

          an11 + bn11
        }
        )
      }
    }
  }

  def VWAPonDeleteBIDS(var_BIDS_T: Rep[Double], var_BIDS_ID: Rep[Long], var_BIDS_BROKER_ID: Rep[Long], var_BIDS_VOLUME: Rep[Double], var_BIDS_PRICE: Rep[Double], VWAP_orig : Rep[SimpleVal[Double]], VWAP_mBIDS1_orig : Rep[K3PersistentCollection[(Double), Double]], VWAP_mBIDS1_L1_1_orig : Rep[SimpleVal[Double]], VWAP_mBIDS1_L2_1_orig : Rep[K3PersistentCollection[(Double), Double]]) {
    val VWAP = VWAP_orig.mutable;

    val VWAP_mBIDS1 = VWAP_mBIDS1_orig.mutable;

    val VWAP_mBIDS1_L1_1 = VWAP_mBIDS1_L1_1_orig.mutable;

    val VWAP_mBIDS1_L2_1 = VWAP_mBIDS1_L2_1_orig.mutable;
    {
      val _var_existing_out_tier:Rep[K3PersistentCollection[(Double), Double]] = VWAP_mBIDS1;

      val var_existing_out_tier:Rep[K3PersistentCollection[(Double), Double]] = _var_existing_out_tier /* Rep[K3PersistentCollection[(Double), Double]] => Rep[K3PersistentCollection[(Double), Double]] */;
      {
        val _var___prod_ret__8:Rep[Double] = {
          val an13 = ({
            val an12 = ((unit(-1L)).asInstanceOf[Rep[Double]]);

            val bn12 = (var_BIDS_PRICE);

            an12 * bn12
          }
          );

          val bn13 = (var_BIDS_VOLUME);

          an13 * bn13
        };

        val var___prod_ret__8:Rep[Double] = _var___prod_ret__8 /* Rep[Double] => Rep[Double] */;
        {
          val nv = {
            val an14 = (if((var_existing_out_tier).contains((var_BIDS_PRICE))) {
              (var_existing_out_tier).lookup((var_BIDS_PRICE)) 
            }
            else {
              unit(0.0) 
            }
            );

            val bn14 = (var___prod_ret__8);

            an14 + bn14
          };

          VWAP_mBIDS1.updateValue((var_BIDS_PRICE), nv)
        }
      }
    };
    {
      val _var_existing_out_tier:Rep[Double] = VWAP_mBIDS1_L1_1.get();

      val var_existing_out_tier:Rep[Double] = _var_existing_out_tier /* Rep[Double] => Rep[Double] */;
      {
        val _var___prod_ret__9:Rep[Double] = {
          val an15 = ((unit(-1L)).asInstanceOf[Rep[Double]]);

          val bn15 = (var_BIDS_VOLUME);

          an15 * bn15
        };

        val var___prod_ret__9:Rep[Double] = _var___prod_ret__9 /* Rep[Double] => Rep[Double] */;

        VWAP_mBIDS1_L1_1.update({
          val an16 = (var_existing_out_tier);

          val bn16 = (var___prod_ret__9);

          an16 + bn16
        }
        )
      }
    };
    {
      val _var_existing_out_tier:Rep[K3PersistentCollection[(Double), Double]] = VWAP_mBIDS1_L2_1;

      val var_existing_out_tier:Rep[K3PersistentCollection[(Double), Double]] = _var_existing_out_tier /* Rep[K3PersistentCollection[(Double), Double]] => Rep[K3PersistentCollection[(Double), Double]] */;
      {
        val _var___prod_ret__10:Rep[Double] = {
          val an17 = ((unit(-1L)).asInstanceOf[Rep[Double]]);

          val bn17 = (var_BIDS_VOLUME);

          an17 * bn17
        };

        val var___prod_ret__10:Rep[Double] = _var___prod_ret__10 /* Rep[Double] => Rep[Double] */;
        {
          val nv = {
            val an18 = (if((var_existing_out_tier).contains((var_BIDS_PRICE))) {
              (var_existing_out_tier).lookup((var_BIDS_PRICE)) 
            }
            else {
              unit(0.0) 
            }
            );

            val bn18 = (var___prod_ret__10);

            an18 + bn18
          };

          VWAP_mBIDS1_L2_1.updateValue((var_BIDS_PRICE), nv)
        }
      }
    };
    {
      val _var_existing_out_tier:Rep[Double] = VWAP.get();

      val var_existing_out_tier:Rep[Double] = _var_existing_out_tier /* Rep[Double] => Rep[Double] */;
      {
        val _var___prod_ret__15:Rep[Double] = (({
          val v = {
            val _var___prod_ret__11:Rep[Double] = {
              val an20 = (VWAP_mBIDS1_L1_1.get());

              val bn20 = (unit(0.250));

              an20 * bn20
            };

            val var___prod_ret__11:Rep[Double] = _var___prod_ret__11 /* Rep[Double] => Rep[Double] */;

            ((var___prod_ret__11),(unit(1L)))
          };

          newSingletonK3IntermediateCollection[(Double), Long](Tuple2((v._1),v._2)) 
        }
        .map((y:Rep[Tuple2[(Double),Long]]) => {
          val v = ({
            (x:Rep[Tuple2[(Double), Long]]) => {
              val var___sql_inline_agg_1:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

              val var___lift_ret__3:Rep[Long] = x._2 /* Rep[Long] => Rep[Long] */;

              VWAP_mBIDS1.map((y:Rep[Tuple2[(Double),Double]]) => {
                val v: Rep[Tuple3[Double,Double,Double]] = ({
                  (x:Rep[Tuple2[(Double), Double]]) => {
                    val var_B1_PRICE:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

                    val var___map_ret__5:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

                    ((var___sql_inline_agg_1),(var_B1_PRICE),({
                      val an21 = ((var___lift_ret__3).asInstanceOf[Rep[Double]]);

                      val bn21 = (var___map_ret__5);

                      an21 * bn21
                    }
                    )) 
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
        )).flatten[Tuple2[Double,Double], Double].map((y:Rep[Tuple2[Tuple2[Double,Double],Double]]) => {
          val v = ({
            (x:Rep[Tuple2[Tuple2[Double,Double], Double]]) => {
              val var___sql_inline_agg_1:Rep[Double] = x._1._1 /* Rep[Double] => Rep[Double] */;

              val var_B1_PRICE:Rep[Double] = x._1._2 /* Rep[Double] => Rep[Double] */;

              val var___prod_ret__13:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;
              {
                val v = {
                  val _var___prod_ret__12:Rep[Double] = VWAP_mBIDS1_L2_1.map((y:Rep[Tuple2[(Double),Double]]) => {
                    val v: Rep[Tuple2[Double,Double]] = ({
                      (x:Rep[Tuple2[(Double), Double]]) => {
                        val var_B2_PRICE:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

                        val var___map_ret__6:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

                        ((var_B2_PRICE),({
                          val an23 = (var___map_ret__6);

                          val bn23 = ((if({
                            val ac2 = (var_B1_PRICE);

                            val bc2 = (var_B2_PRICE);

                            ac2 < bc2
                          }
                          ) unit(1.0) else unit(0.0)));

                          an23 * bn23
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

                      val var___prod_ret__12:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

                      (var___accv_3:Rep[Double]) => {                            {
                          val an22 = (var___accv_3);

                          val bn22 = (var___prod_ret__12);

                          an22 + bn22
                        }
                      }
                    }
                  }
                  );

                  val var___prod_ret__12:Rep[Double] = _var___prod_ret__12 /* Rep[Double] => Rep[Double] */;

                  ((var___prod_ret__12),(unit(1L)))
                };

                newSingletonK3IntermediateCollection[(Double), Long](Tuple2((v._1),v._2)) 
              }
              .map((y:Rep[Tuple2[(Double),Long]]) => {
                val v: Rep[Tuple4[Double,Double,Double,Double]] = ({
                  (x:Rep[Tuple2[(Double), Long]]) => {
                    val var___sql_inline_agg_2:Rep[Double] = x._1 /* Rep[Double] => Rep[Double] */;

                    val var___lift_ret__4:Rep[Long] = x._2 /* Rep[Long] => Rep[Long] */;

                    ((var___sql_inline_agg_1),(var_B1_PRICE),(var___sql_inline_agg_2),({
                      val an24 = (var___prod_ret__13);

                      val bn24 = ((var___lift_ret__4).asInstanceOf[Rep[Double]]);

                      an24 * bn24
                    }
                    )) 
                  }
                }
                )(y);

                make_tuple2(make_tuple3(v._1,v._2,v._3), v._4) 
              }
              ) 
            }
          }
          )(y);

          v 
        }
        )).flatten[Tuple3[Double,Double,Double], Double].map((y:Rep[Tuple2[Tuple3[Double,Double,Double],Double]]) => {
          val v: Rep[Tuple4[Double,Double,Double,Double]] = ({
            (x:Rep[Tuple2[Tuple3[Double,Double,Double], Double]]) => {
              val var___sql_inline_agg_1:Rep[Double] = x._1._1 /* Rep[Double] => Rep[Double] */;

              val var_B1_PRICE:Rep[Double] = x._1._2 /* Rep[Double] => Rep[Double] */;

              val var___sql_inline_agg_2:Rep[Double] = x._1._3 /* Rep[Double] => Rep[Double] */;

              val var___prod_ret__14:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

              ((var___sql_inline_agg_1),(var_B1_PRICE),(var___sql_inline_agg_2),({
                val an25 = (var___prod_ret__14);

                val bn25 = ((if({
                  val ac3 = (var___sql_inline_agg_2);

                  val bc3 = (var___sql_inline_agg_1);

                  ac3 < bc3
                }
                ) unit(1.0) else unit(0.0)));

                an25 * bn25
              }
              )) 
            }
          }
          )(y);

          make_tuple2(make_tuple3(v._1,v._2,v._3), v._4) 
        }
        ).fold(unit(0.0), {
          (x:Rep[Tuple2[(Tuple3[Double,Double,Double]), Double]]) => {
            val var___sql_inline_agg_1:Rep[Double] = x._1._1 /* Rep[Double] => Rep[Double] */;

            val var_B1_PRICE:Rep[Double] = x._1._2 /* Rep[Double] => Rep[Double] */;

            val var___sql_inline_agg_2:Rep[Double] = x._1._3 /* Rep[Double] => Rep[Double] */;

            val var___prod_ret__15:Rep[Double] = x._2 /* Rep[Double] => Rep[Double] */;

            (var___accv_4:Rep[Double]) => {                  {
                val an19 = (var___accv_4);

                val bn19 = (var___prod_ret__15);

                an19 + bn19
              }
            }
          }
        }
        );

        val var___prod_ret__15:Rep[Double] = _var___prod_ret__15 /* Rep[Double] => Rep[Double] */;

        VWAP.update({
          val an26 = (unit(0.0));

          val bn26 = (var___prod_ret__15);

          an26 + bn26
        }
        )
      }
    }
  }
}

class TestTupledGeneratorHuge extends FileDiffSuite {

  val prefix = "test-out/epfl/test15-"

  def testgenerator2 = {
    withOutFile(prefix+"tupled-generator-huge"){
       new TupledGeneratorHugeProg with GeneratorOpsExp with NumericOpsExp
        with OrderingOpsExp with PrimitiveOpsExp with EqualExp
        with StructExp with StructExpOptCommon with ArrayOpsExp
        with MiscOpsExp with ScalaCompile with K3PersistentCollectionExp
        with ListContainerExp with SimpleValExp with ExtendedExpressions { self =>

        val printWriter = new java.io.PrintWriter(System.out)

        //test1: first "loop"
        val codegen = new ScalaGenGeneratorOps with ScalaGenNumericOps
          with ScalaGenOrderingOps with ScalaGenPrimitiveOps with ScalaGenEqual
          with ScalaGenArrayOps with ScalaGenStruct with ScalaGenMiscOps
          with ScalaGenK3PersistentCollection with ScalaGenListContainer
          with ScalaGenSimpleVal with ScalaConciseCodegen{ val IR: self.type = self }

        

        codegen.emitSource9(VWAPonInsertBIDS _ , "VWAPonInsertBIDS", printWriter)
        codegen.emitDataStructures(printWriter)

        codegen.emitSource9(VWAPonDeleteBIDS _ , "VWAPonDeleteBIDS", printWriter)
        codegen.emitDataStructures(printWriter)


      }
    }
    assertFileEqualsCheck(prefix+"tupled-generator-huge")
  }
}