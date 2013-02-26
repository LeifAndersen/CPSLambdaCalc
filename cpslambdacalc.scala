// Language:
// <cexp> ::= (<aexp> <aexp>*)
//         |   <hault>
// <aexp> ::= (λ (<var>*) <cexp>)
//         | <var>
//

abstract class Exp { }

abstract class CExp extends Exp { }

case class ListExp(prog: AExp, arg: List[AExp]) extends CExp { }

case class HaultExp() extends CExp { }

abstract class AExp() extends Exp { }

case class LambExp(param: List[VarExp], body: CExp) extends AExp { }

case class VarExp(value: String) extends AExp { }

case class Closure(e: Exp, env: Map[VarExp, Closure]) extends Exp { }

object Analysis {

  def eval(e: Exp, env: Map[VarExp, Closure]): Closure = e match {
    case ListExp(prog, arg) => apply(eval(prog, env), for(i <- arg) yield eval(i, env));
    case LambExp(param, body) => Closure(e, env);
    case a: VarExp => env(a);
  }

  def apply(f: Exp, x: List[Closure]): Closure = f match {
    case Closure(LambExp(param, body), env) => eval(body, env ++ (for(List(i: VarExp, j: Closure) <- param.zip(x)) yield (i -> j)));
  }

  def main(args: Array[String]) {
    println("Hello World!");
  }
}