package com.leifandersen.cpslambdacalc

import scala.actors.Actor
import scala.actors.Actor._

import Scalify._

// Language:
// <cexp> ::= (<aexp> <aexp>*)
// <aexp> ::= (λ (<var>*) <cexp>)
//         | <var>
//         | <halt>

object CPSLambdaCalc {

  def closureToEval(c: Closure, s: Store): EvalState = c match {
    case Closure(e, env) => EvalState(e, env, s);
  }

  def aeval(e: Exp, env: Map[VarExp, Address], store: Store): Set[Closure] = e match {
    case LambExp(param, body) => Set(Closure(e, env));
    case a: VarExp => store.alookup(env(a));
    case a: HaltExp => Set[Closure]();
  }

  def aapply(f: Closure, x: List[Set[Closure]], store: Store): EvalState = f match {
    case Closure(LambExp(param, body), env) => {
//      var s = Store.copy(store)
      var s = store
//      EvalState(body, env ++ param.zipAll(for(i <- x) yield s.aextend(i), VarExp("null"), Address(0)), s);
      EvalState(body, env ++ param.zip(for(i <- x) yield s.aextend(Address(f.e), i)), s);
      //EvalState(body, env ++ param.zip(for(i <- x) yield s.aextend(Address(HaltExp()), i)), s);
    }
  }

  def aevalState(state: State): Set[Closure] = state match {
    case EvalState(e, env, s) => for(i <- aeval(e, env, s)) yield i;
  }

  def astep(state: State): Set[State] = state match {
    case EvalState(ApplyExp(prog, arg), env, s) =>
      Set(ApplyState(EvalState(prog, env, s), for(i <- arg) yield EvalState(i, env, s), s));
    case EvalState(e, env, s) => for(i <- aeval(e, env, s)) yield closureToEval(i, s);
    case ApplyState(f, x, s) => {
      val tmpProducers = for(c <- x) yield new EvalProducer(c);
      val b = for (c <- tmpProducers) yield {
        var tmpSet = Set[Closure]();
        for(i <- c.iterator) {
          tmpSet ++= i
        }
        tmpSet
      }
      for(a <- aevalState(f); if (a.e.isInstanceOf[LambExp]); if (a.e.asInstanceOf[LambExp].param.length == b.length)) yield aapply(a, b, s);
    }
    case HaltState() => Set(HaltState());
  }

  def anaivestep(state: State): Set[State] = state match {
    case EvalState(ApplyExp(prog, arg), env, s) =>
      Set(ApplyState(EvalState(prog, env, s), for(i <- arg) yield EvalState(i, env, s), s));
    case EvalState(e, env, s) => for(i <- aeval(e, env, s)) yield closureToEval(i, s);
    case ApplyState(f, x, s) => {
      val b = for (c <- x) yield aevalState(c)
      for(a <- aevalState(f); if (a.e.isInstanceOf[LambExp]); if (a.e.asInstanceOf[LambExp].param.length == b.length)) yield aapply(a, b, s);
    }
    case HaltState() => Set(HaltState());
  }

  def ainject(code: Exp): EvalState = {
    return EvalState(code, Map[VarExp,Address](), Store())
  }

  def afix(in: Map[State,Set[State]]): Map[State,Set[State]] = {
    var next = in;
    var producers = new Iterator[(State,StateProducer)] {
      var internal = List[(State,StateProducer)]()
      def hasNext: Boolean = {
        internal.length > 0
      }
      def next: (State, StateProducer) = {
        val tmp = internal(0)
        internal = internal.tail
        return tmp
      }
      def extend(a: Set[(State,StateProducer)]) {
        internal ++= a
      }
      def extendList(a: List[(State,StateProducer)]) {
        internal ++= a
      }
    }
    def getProducers(in: Set[State]) {
      val t = for(j <- in; if (!next.contains(j))) yield
        (j -> new StateProducer(j))
      producers.extend(t)
    }

    for(i <- in) {
      getProducers(i._2)
    }
    for(i <- producers) {
      var tmpStep = Set[State]()
      for(j <- i._2.iterator) {
        tmpStep ++= j
      }
      next += (i._1 -> tmpStep)
      getProducers(tmpStep)
    }
    if(in == next) {
      return next;
    } else {
      return afix(next);
    }
  }

  def anaivefix(in: Map[State,Set[State]]): Map[State,Set[State]] = {
    var next = in;
    for(i <- in.values; j <- i; if(!next.contains(j))) {
      val step = anaivestep(j);
      next += (j -> step);
    }
    if(in == next) {
      return next;
    } else {
      return anaivefix(next);
    }
  }

  def arun(in: State): Map[State,Set[State]] = {
    val step = astep(in);
    afix(Map(in->step));
  }

  def anaiverun(in: State): Map[State,Set[State]] = {
    val step = anaivestep(in);
    anaivefix(Map(in->step));
  }

  def analyze(code: Exp) {
    val startState = ainject(code);
    val result = arun(startState);
  }

  def analyzeAndGenerate(code: Exp): String = {
    val startState = ainject(code);
    val result = arun(startState);
    val dot = Dotify.dotify(result);
    return dot;
  }

  def analyzeNaive(code: Exp) {
    val startState = ainject(code);
    val result = anaiverun(startState);
  }

  def analyzeNaiveAndGenerate(code: Exp): String = {
    val startState = ainject(code);
    val result = anaiverun(startState);
    val dot = Dotify.dotify(result);
    return dot;
  }
}
