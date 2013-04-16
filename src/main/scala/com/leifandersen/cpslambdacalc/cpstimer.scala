package com.leifandersen.cpslambdacalc

import CPSLambdaCalc._
import Scalify._

object CPSTimer extends App {

  def timeThread(code: Exp, i: Int): List[(Long, Long)] = {
    var results = List[(Long, Long)]();
    for(i <- 0 to i) {
      val start = System.currentTimeMillis;
      analyze(code);
      val end = System.currentTimeMillis;
      results ++= List((start, end));
    }
    return results;
  }

  def timeNaive(code: Exp, i: Int): List[(Long, Long)] = {
    var results = List[(Long, Long)]();
    for(i <- 0 to i) {
      val start = System.currentTimeMillis;
      analyzeNaive(code);
      val end = System.currentTimeMillis;
      results ++= List((start, end));
    }
    return results;
  }

  def avergeRunTime(in: List[(Long, Long)]): Long = {
    val diffs = for(i <- in) yield i._2 - i._1;
    var sum:Long = 0;
    for(i <- diffs) {
      sum += i
    }
    return sum/diffs.length;
  }

  val code = CodeSamples.fact20;
  val reps = 20;
  val startup = timeThread(code, reps);
  val thread = timeThread(code, reps);
  val startup2 = timeNaive(code, reps);
  val naive = timeNaive(code, reps);
  println(avergeRunTime(thread));
  println();
  println(avergeRunTime(naive));
}
