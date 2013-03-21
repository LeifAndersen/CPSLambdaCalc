package com.leifandersen.cpslambdacalc

// Language:
// <cexp> ::= (<aexp> <aexp>*)
//         |   <hault>
// <aexp> ::= (λ (<var>*) <cexp>)
//         | <var>
//

abstract class Exp { }

abstract class CExp extends Exp { }

case class ApplyExp(prog: AExp, arg: List[AExp]) extends CExp { }

case class HaultExp() extends CExp { }

abstract class AExp() extends Exp { }

case class LambExp(param: List[VarExp], body: CExp) extends AExp { }

case class VarExp(value: String) extends AExp { }


case class Closure(e: Exp, env: Map[VarExp, Address]) { }


case class Address(address: Int) { }


abstract class State { }

case class EvalState(e: Exp, env: Map[VarExp, Address]) extends State { }

case class ApplyState(f: EvalState, x: List[EvalState]) extends State { }

case class HaultState() extends State { }

object Analysis extends App {

  var store = Map[Address, EvalState]();
  var id = 0;

  var aStore = Map[Address, Set[EvalState]]();
  val maxId = 10;

  var states = Set[State]();

  def aextend(e: EvalState): Address = {
    val addr = Address(id);
    if(aStore.contains(addr)) {
      aStore = aStore + (addr -> (aStore(addr) + e));
    } else {
      aStore += (addr -> Set(e));
    }
    id = (id + 1) % maxId;
    return addr;
  }

  def alookup(e: Address): Set[EvalState] = {
    return aStore(e);
  }

  def aeval(e: Exp, env: Map[VarExp, Address]): Set[State] = e match {
    case LambExp(param, body) => Set(EvalState(e, env));
    case a: VarExp => Set[State]() ++ alookup(env(a));
    case a: HaultExp => Set(HaultState());
  }

  def aapply(f: EvalState, x: List[EvalState]): State = f match {
    case EvalState(LambExp(param, body), env) => EvalState(body, env ++ param.zip(for(i <- x) yield aextend(i)));
  }

  def astep(state: State): Set[State] = state match {
    case EvalState(ApplyExp(prog, arg), env) => Set(ApplyState(EvalState(prog, env),
                                                               for(i <- arg) yield EvalState(i, env)));
    case EvalState(e, env) => aeval(e, env);
    case ApplyState(f, x) => Set(aapply(f, x));
    case HaultState() => Set(HaultState());
  }

  def ainject(code: Exp): EvalState = {
    return EvalState(code, Map[VarExp,Address]())
  }

  def afix(in: Set[State]): Set[State] = {
    var next = in;
    for(i <- in) {
      next ++= astep(i);
    };
    if (in == next) {
      return next;
    } else {
      return afix(next);
    }
  }

  val code = ApplyExp(LambExp(List(VarExp("x")), HaultExp()),
                      List(LambExp(List(VarExp("x")), HaultExp())));
  System.out.println(code);
  val startState = ainject(code);
  states += startState;
  val result = afix(states);
  System.out.println("Start State:");
  System.out.println(startState);
  System.out.println("\nResulting State:");
  System.out.println(result);
}
