package com.leifandersen.cpslambdacalc

import scala.actors.Actor
import scala.actors.Actor._

// Language:
// <cexp> ::= (<aexp> <aexp>*)
//         |   <halt>
// <aexp> ::= (λ (<var>*) <cexp>)
//         | <var>
//

object Analysis extends App {

  def closureToEval(c: Closure, s: Store): EvalState = c match {
    case Closure(e, env) => EvalState(e, env, s);
  }

  def aeval(e: Exp, env: Map[VarExp, Address], store: Store): Set[Closure] = e match {
    case LambExp(param, body) => Set(Closure(e, env));
    case a: VarExp => store.alookup(env(a));
    //case a: HaltExp => Set(HaltState());
    case a: HaltExp => Set[Closure]();
  }

  def aapply(f: Closure, x: List[Set[Closure]], store: Store): Closure = f match {
    case Closure(LambExp(param, body), env) => Closure(body, env ++ param.zip(for(i <- x) yield store.aextend(i)));
  }

  def aevalState(state: State): Set[Closure] = state match {
    case EvalState(e, env, s) => for(i <- aeval(e, env, s)) yield i;
  }

  def applyList(in: List[Set[Closure]]): Set[List[Closure]] = {
    if(in.length == 0) {
      return Set[List[Closure]](List[Closure]());
    } else {
      return for(i <- in(0); j <- applyList(in.tail)) yield List(i) ++ j;
    }
  }

  def astep(state: State): Set[State] = state match {
    case EvalState(ApplyExp(prog, arg), env, s) =>
      Set(ApplyState(EvalState(prog, env, s), for(i <- arg) yield EvalState(i, env, s), s));
    case EvalState(e, env, s) => for(i <- aeval(e, env, s)) yield closureToEval(i, s);
    case ApplyState(f, x, s) =>
      val b = for (c <- x) yield aevalState(c)
      for(a <- aevalState(f)) yield closureToEval(aapply(a, b, s), s);
    case HaltState() => Set(HaltState());
  }

  def ainject(code: Exp, i: Int): EvalState = {
    return EvalState(code, Map[VarExp,Address](), Store(i))
  }

  def afix(in: Map[State,Set[State]]): Map[State,Set[State]] = {
    var next = in;
    var producers = Map[State, StateProducer]();
    for(i <- in.values; j <- i) {
      if(!next.contains(j)) {
        producers += (j -> new StateProducer(j))
//        var step = astep(j);
//        next += (j -> step);
      }
    }
    for(i <- producers.keys) {
      var tmpStep = Set[State]();
      for(j <- producers(i).iterator) {
        tmpStep ++= j
      }
      next += (i -> tmpStep);
    }
    if(in == next) {
      return next;
    } else {
      return afix(next);
    }
  }

  def arun(in: State): Map[State,Set[State]] = {
    val step = astep(in);
    afix(Map(in->step));
  }


//  val code = ApplyExp(LambExp(List(VarExp("x")), HaltExp()),
//                      List(LambExp(List(VarExp("x")), HaltExp())));
//  val code = LambExp(List(VarExp("x"), VarExp("k")), ApplyExp(VarExp("k"), List(VarExp("x"))));
//  val code = ApplyExp(LambExp(List(VarExp("x"), VarExp("k")),
//                              ApplyExp(VarExp("k"), List(VarExp("x"), VarExp("k")))),
//                      List(LambExp(List(VarExp("a"), VarExp("b")),
//                                   ApplyExp(VarExp("b"), List(VarExp("a"), VarExp("b")))),
//                           LambExp(List(VarExp("q"), VarExp("w")),
//                                   ApplyExp(VarExp("w"), List(VarExp("q"), VarExp("w"))))));
  val code = ApplyExp(LambExp(List(VarExp("x")), ApplyExp(VarExp("x"), List(VarExp("x")))),
                      List(LambExp(List(VarExp("x")), ApplyExp(VarExp("x"), List(VarExp("x"))))));
//  val code = ApplyExp(LambExp(List(VarExp("x"), VarExp("k")),
//                              ApplyExp(VarExp("k"), List(VarExp("x")))),
//                      List(LambExp(List[VarExp](), HaltExp()),
//                           LambExp(List(VarExp("a")),
//                                   ApplyExp(VarExp("a"), List[VarExp]()))));

  val storeSize = 5;
  val startState = ainject(code, storeSize);
  val result = arun(startState);
  val dot = Dotify.dotify(result);
  println(dot);
}
