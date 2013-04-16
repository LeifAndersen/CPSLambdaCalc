package com.leifandersen.cpslambdacalc

import CPSLambdaCalc._
import Scalify._

object CPSDotter extends App {
  println(analyzeNaiveAndGenerate(CodeSamples.fact2, 50))
}
