package com.leifandersen.cpslambdacalc

// Language:
// <cexp> ::= (<aexp> <aexp>*)
//        ::=  <aexp>
//         |   <hault>
// <aexp> ::= (λ (<var>*) <cexp>)
//         | <var>
//

abstract class Exp { }

abstract class CExp extends Exp { }

case class ApplyExp(prog: AExp, arg: List[AExp]) extends CExp { }

case class HaultExp() extends CExp { }

abstract class AExp() extends CExp { }

case class LambExp(param: List[VarExp], body: CExp) extends AExp { }

case class VarExp(value: String) extends AExp { }


// case class Closure(e: Exp, env: Map[VarExp, Address]) { }


case class Address(address: Int) { }


abstract class State { }

case class EvalState(e: Exp, env: Map[VarExp, Address]) extends State { }

case class ApplyState(f: EvalState, x: List[EvalState]) extends State { }

case class HaultState() extends State { }

object Analysis extends App {

  var store = Map[Address, EvalState]();
  var id = 0;

  var aStore = Map[Address, List[EvalState]]();
  val maxId = 10;

  def aextend(e: EvalState): Address = {
    val addr = Address(id);
    if(aStore.contains(addr)) {
      aStore = aStore + (addr -> (aStore(addr) ::: List(e)));
    } else {
      aStore += (addr -> List(e));
    }
    id = (id + 1) % maxId;
    return addr;
  }

  def alookup(e: Address): List[EvalState] = {
    return aStore(e);
  }

  def extend(e: EvalState): Address = {
    val addr = Address(id);
    store += (addr -> e);
    id = id + 1;
    return addr;
  }

  def lookup(e: Address): EvalState = {
    return store(e);
  }

  def eval(e: Exp, env: Map[VarExp, Address]): EvalState = e match {
    case ApplyExp(prog, arg) => step(ApplyState(eval(prog, env),
                                                for(i <- arg) yield eval(i, env)));
    case LambExp(param, body) => EvalState(e, env);
    case a: VarExp => lookup(env(a));
    case a: HaultExp => step(HaultState());
  }

  def apply(f: EvalState, x: List[EvalState]): EvalState = f match {
    case EvalState(LambExp(param, body), env) => EvalState(body, env ++ param.zip(for(i <- x) yield extend(i)));
  }

  def step(state: State): EvalState = state match {
    case EvalState(e: Exp, env: Map[VarExp, Address]) => eval(e, env);
    case ApplyState(f: EvalState, x: List[EvalState]) => apply(f, x);
    case HaultState() => null;
  }

  def inject(code: Exp): State = {
    EvalState(code, Map[VarExp, Address]());
  }

  var code = ApplyExp(LambExp(List(VarExp("x")), VarExp("x")),
                      List(LambExp(List(VarExp("x")), VarExp("x"))));
  eval(code, Map[VarExp, Address]());

}
