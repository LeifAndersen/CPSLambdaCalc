package com.leifandersen.cpslambdacalc

import languages.sexp._
import SExpSyntax._

object Scalify {

  def scalify(in: String): Exp = {
    val parsed = SParser.parse(in)
    if(parsed.length == 1) {
      return scalifySExp(parsed(0))
    } else if(parsed.length == 0) {
      throw new Exception("No Expressions: " + in)
    } else {
      throw new Exception("Multiple Expressions: " + in)
    }
  }

  def scalifySExp(in: SExp): Exp = in match {
    case L(arg) => scalifyList(arg)
    case S("Halt") => HaltExp()
    case S("HALT") => HaltExp()
    case S("halt") => HaltExp()
    case S(symbol) => VarExp(symbol);
    case Z(z) => VarExp(z.toString);
  }

  def scalifyList(in: List[SExp]): Exp = in match {
    case List(S("λ"), L(args), body) =>
      LambExp(for(i <- args) yield scalifyArg(i), scalifyBody(body))
    case List(S("lambda"), L(args), body) =>
      LambExp(for(i <- args) yield scalifyArg(i), scalifyBody(body))
    case S("λ") :: _ => null
    case S("lambda") :: _ => null
    case func :: tail =>
      ApplyExp(scalifyAExp(func), for(i <- tail) yield scalifyAExp(i))
    case _ => throw new Exception("Not a valid List Expression: " + in)
  }

  def scalifyArg(in: SExp): VarExp = in match {
    case S(symbol) => VarExp(symbol);
    case Z(z) => VarExp(z.toString);
  }

  def scalifyAExp(in: SExp): AExp = in match {
    case L(List(S("λ"), L(args), body)) =>
      LambExp(for(i <- args) yield scalifyArg(i), scalifyBody(body))
    case L(List(S("lambda"), L(args), body)) =>
      LambExp(for(i <- args) yield scalifyArg(i), scalifyBody(body))
    case S("Halt") => HaltExp()
    case S("HALT") => HaltExp()
    case S("halt") => HaltExp()
    case S(symbol) => VarExp(symbol);
    case Z(z) => VarExp(z.toString);
    case _ => throw new Exception("Not a valid AExp: " + in)
  }

  def scalifyBody(in: SExp): CExp = in match {
    case L(func :: tail) =>
      ApplyExp(scalifyAExp(func), for(i <- tail) yield scalifyAExp(i))
    case _ => throw new Exception("Not a valid CExp: " + in)
  }

}

