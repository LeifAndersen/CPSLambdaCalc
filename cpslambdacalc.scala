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

object Analysis {

  var code = ApplyExp(LambExp(List(VarExp("x")), HaultExp()), List(VarExp("x")));

  var store = Map[Address, Closure]();
  var id = 0;

  def extend(e: Closure): Address = {
    val addr = Address(id);
    store += (addr -> e);
    id = id + 1;
    return addr;
  }

  def lookup(e: Address): Closure = {
    return store(e);
  }

  def eval(e: Exp, env: Map[VarExp, Address]): Closure = e match {
    case ApplyExp(prog, arg) => apply(eval(prog, env), for(i <- arg) yield eval(i, env));
    case LambExp(param, body) => Closure(e, env);
    case a: VarExp => lookup(env(a));
    case a: HaultExp => null;
  }

  def apply(f: Closure, x: List[Closure]): Closure = f match {
    case Closure(LambExp(param, body), env) => eval(body, env ++ param.zip(for(i <- x) yield extend(i)));
  }

  def main(args: Array[String]) {
    eval(code, Map[VarExp, Address]());
  }
}
