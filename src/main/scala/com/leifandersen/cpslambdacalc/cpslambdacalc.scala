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

  var id = 0;

  var aStore = Map[Address, Set[Closure]]();
  val maxId = 10;

  def aextend(e: Closure): Address = {
    val addr = Address(id);
    if(aStore.contains(addr)) {
      aStore = aStore + (addr -> (aStore(addr) + e));
    } else {
      aStore += (addr -> Set(e));
    }
    id = (id + 1) % maxId;
    return addr;
  }

  def alookup(e: Address): Set[Closure] = {
    return aStore(e);
  }

  def closureToEval(c: Closure): EvalState = c match {
    case Closure(e, env) => EvalState(e, env);
  }

  def aeval(e: Exp, env: Map[VarExp, Address]): Set[Closure] = e match {
    case LambExp(param, body) => Set(Closure(e, env));
    case a: VarExp => alookup(env(a));
    //case a: HaltExp => Set(HaltState());
    case a: HaltExp => Set[Closure]();
  }

  def aapply(f: Closure, x: List[Closure]): Closure = f match {
    case Closure(LambExp(param, body), env) => Closure(body, env ++ param.zip(for(i <- x) yield aextend(i)));
  }

  def aevalState(state: State): Set[Closure] = state match {
    case EvalState(e, env) => for(i <- aeval(e, env)) yield i;
  }

  def applyList(in: List[Set[Closure]]): Set[List[Closure]] = {
    if(in.length == 0) {
      return Set[List[Closure]](List[Closure]());
    } else {
      return for(i <- in(0); j <- applyList(in.tail)) yield List(i) ++ j;
    }
  }

  def astep(state: State): Set[State] = state match {
    case EvalState(ApplyExp(prog, arg), env) => Set(ApplyState(EvalState(prog, env),
                                                               for(i <- arg) yield EvalState(i, env)));
    case EvalState(e, env) => for(i <- aeval(e, env)) yield closureToEval(i);
    case ApplyState(f, x) => for(a <- aevalState(f); b <- applyList(for (c <- x) yield (aevalState(c))))
                             yield closureToEval(aapply(a, b));
    case HaltState() => Set(HaltState());
  }

  def ainject(code: Exp): EvalState = {
    return EvalState(code, Map[VarExp,Address]())
  }

  def afix(in: Map[State,Set[State]]): Map[State,Set[State]] = {
    var next = in;
    for(i <- in.values; j <- i) {
      if(!next.contains(j)) {
        var step = astep(j);
        next += (j -> step);
      }
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
//                              ApplyExp(VarExp("k"), List(VarExp("x"), VarExp("x")))),
//                      List(LambExp(List[VarExp](), HaltExp()),
//                           LambExp(List(VarExp("a"), VarExp("b")),
//                                   ApplyExp(VarExp("a"), List[VarExp]()))));

  val startState = ainject(code);
  val result = arun(startState);
  val dot = Dotify.dotify(result);
  println(dot);
}
