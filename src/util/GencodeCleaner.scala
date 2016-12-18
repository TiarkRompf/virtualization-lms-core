package scala.lms.util

import java.util.regex.Pattern
import scala.collection.mutable.ListBuffer

object CodegenCleaner {
    val pattern1 = Pattern.compile("val x[0-9]* = x[0-9]*$")
    val pattern2 = Pattern.compile("var x[0-9]* = ")
    val pattern3 = Pattern.compile("val x[0-9]* =")
    val pattern4 = Pattern.compile("var x[0-9]* = x[0-9]*$")
    val pattern5 = Pattern.compile("x[0-9]*$")

    def clean(src: String) = {
        var lines = src.split("\n").map(x => x.trim)
        // Extract variables
        val variables = lines.filter(x => pattern2.matcher(x).find).map(x => {
            val y = x.split("=")
            ( y(0).replaceAll("var ","").trim, y(1).trim )
        }).sortBy(x => x._1)
        // Extract values
        val values = lines.filter(x => pattern3.matcher(x).find).map(x => {
            val y = x.split("=")
            ( y(0).replaceAll("val ","").trim, y(1).trim )
        }).sortBy(x => x._1)
        // Extract "val x = x" lines
        var resList = new ListBuffer[(String,String)]()
        lines = lines.map(x =>
            if (pattern1.matcher(x).find) {
                val y = x.split("=")
                val valId = y(0).replaceAll("val ","").trim
                val lhs = y(1).trim
                variables.find(z => z._1 == lhs) match {
                    case Some(w) => {
                        resList += new Tuple2[String,String](valId,lhs)
                        ""
                    }
                    case None => x
                }
            } else x
        )
//        println("PHASE 1A DONE")
        // Now remove all references to this val
        resList.foreach( res => {
            lines = lines.map( line => line.replaceAll(res._1 + "\\.", res._2 + ".").replaceAll(res._1 + "$",res._2).replaceAll(res._1 + " ", res._2 + " ").replaceAll(res._1 + "\\(",res._2 + "(").replaceAll(res._1 + "\\+",res._2 + "+").replaceAll(res._1 + "\\)",res._2 + ")").replaceAll("= __" + res._1 + "Size", "= __" + res._2 + "Size").replaceAll("^__" + res._1 + "Size", "__" + res._2 + "Size").replaceAll("< __" + res._1 + "Size", "< __" + res._2 + "Size").replaceAll("__" + res._1 + "Indices", "__" + res._2 + "Indices").replaceAll("__" + res._1 + "LastIndex", "__" + res._2 + "LastIndex") )
        })

        // Extract "var x = x" lines
        resList = new ListBuffer[(String,String)]()
        lines = lines.map(x =>
            if (pattern4.matcher(x).find) {
                val y = x.split("=")
                val valId = y(0).replaceAll("var ","").trim
                val lhs = y(1).trim
                variables.find(z => z._1 == lhs) match {
                    case Some(w) => {
                        resList += new Tuple2[String,String](valId,lhs)
                        ""
                    }
                    case None => x
                }
            } else x
        )
//        println("PHASE 1A DONE")
        // Now remove all references to this val
        resList.foreach( res => {
            lines = lines.map( line => line.replaceAll(res._1 + "\\.", res._2 + ".").replaceAll(res._1 + "$",res._2).replaceAll(res._1 + " ", res._2 + " ").replaceAll(res._1 + "\\(",res._2 + "(").replaceAll(res._1 + "\\+",res._2 + "+").replaceAll(res._1 + "\\)",res._2 + ")").replaceAll("= __" + res._1 + "Size", "= __" + res._2 + "Size").replaceAll("^__" + res._1 + "Size", "__" + res._2 + "Size").replaceAll("< __" + res._1 + "Size", "< __" + res._2 + "Size").replaceAll("__" + res._1 + "Indices", "__" + res._2 + "Indices").replaceAll("__" + res._1 + "LastIndex", "__" + res._2 + "LastIndex") )
        })


//        println("PHASE 1B DONE")
        // CASE 2
        resList = new ListBuffer[(String,String)]()
        lines = lines.map( line => {
            if (pattern4.matcher(line).find) {
                val y = line.split("=")
                val varId = y(0).replaceAll("var ","").trim
                val lhs = y(1).trim
                values.find(z => z._1 == lhs) match {
                    case Some(w) => {
                        resList += new Tuple2[String,String](varId,lhs)
                        ""
                    }
                    case None => line
                }
            } else line
        })
//        println("PHASE 2A DONE")
        // Now change the val to var
        resList.foreach( res => {
            lines = lines.map( line => line.replaceAll("val " + res._2 + " ","var " + res._1 + " "))
        })

resList.foreach( res => {
            lines = lines.map( line => line.replaceAll(res._1 + "\\.", res._2 + ".").replaceAll(res._1 + "$",res._2).replaceAll(res._1 + " ", res._2 + " ").replaceAll(res._1 + "\\(",res._2 + "(").replaceAll(res._1 + "\\+",res._2 + "+").replaceAll(res._1 + "\\)",res._2 + ")").replaceAll("= __" + res._1 + "Size", "= __" + res._2 + "Size").replaceAll("^__" + res._1 + "Size", "__" + res._2 + "Size").replaceAll("< __" + res._1 + "Size", "< __" + res._2 + "Size").replaceAll("__" + res._1 + "Indices", "__" + res._2 + "Indices").replaceAll("__" + res._1 + "LastIndex", "__" + res._2 + "LastIndex") )
        })


//        println("PHASE 2B DONE")
        // CASE 3
        for (i <- 1 to lines.length - 1) {
            if (lines(i).matches("x[0-9]*$") && pattern3.matcher(lines(i-1)).find) {
                if (lines(i-1).startsWith("val " + lines(i))) {
                    val lhs = lines(i-1).split("=").drop(1).mkString("=")
                    lines(i-1) = lhs
                    lines(i) = ""
                }
            }
        }
//        println("PHASE 3 DONE")

        // print result
        lines = lines.filter(x => x!= "" && x!="()")
//        println("PHASE 4 DONE")
        lines.mkString("\n")
    }
}

