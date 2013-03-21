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

  def arun(in: State): Set[State] = {
    afix(Set(in));
  }

  def dotify(in: Set[State]): String = {
    var i = 0;
    def dotifyState(state: State): String = state match {
      case EvalState(e, env) => "s" + i + "[label=\"Eval State\"];\n";
      case ApplyState(f, x) => "s" + i + "[label=\"Apply State\"];\n";
      case HaultState() => "s" + i + "[label=\"Hault State\"];\n"
    }
    var text = "digraph G {\n" + in.foldLeft("")((x, y) => { i += 1; x + dotifyState(y) }) + "}\n";
    return text;
  }

  val code = ApplyExp(LambExp(List(VarExp("x")), HaultExp()),
                      List(LambExp(List(VarExp("x")), HaultExp())));
//  val code = LambExp(List(VarExp("x"), VarExp("k")), ApplyExp(VarExp("k"), List(VarExp("x"))));
//  val code = ApplyExp(LambExp(List(VarExp("x"), VarExp("k")), ApplyExp(VarExp("k"), List(VarExp("x")))),
//                      List(LambExp(List(VarExp("x"), VarExp("k")), ApplyExp(VarExp("k"), List(VarExp("x")))),
//                           LambExp(List(), HaultExp())));

  val startState = ainject(code);
  val result = arun(startState);
  val dot = dotify(result);
  println(dot);
}
