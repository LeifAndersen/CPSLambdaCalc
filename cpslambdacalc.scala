// Language:
// <cexp> ::= (<aexp> <aexp>)
//         |   <hault>
// <aexp> ::= (λ (<var> <var>) <cexp>)
//         | <var>
//
// Broken Down to:
// <cexp>    ::= <ListExp> | <HaultExp>
// <ListExp> ::= List(<aexp>)
// <aexp> ::= <LambExp> | <VarExp>

abstract class Exp { }

abstract class CExp extends Exp { }

case class ListExp(prog: AExp, arg: AExp) extends CExp { }

case class HaultExp() extends CExp { }

abstract class AExp() extends Exp { }

case class LambExp(cont: Exp, param: VarExp, body: CExp) extends AExp { }

case class VarExp(value: String) extends AExp { }

case class Closure(e: Exp, env: Map[VarExp, Any])

object Analysis {
  var code = ListExp(VarExp("3"), VarExp("3"));

  def eval(e: Exp, env: Map[VarExp, Any]): Any = e match {
    case ListExp(prog, arg) => apply(eval(prog, env).asInstanceOf[Closure], eval(arg, env).asInstanceOf[Exp]);
    case LambExp(cont, pram, body) => eval(cont, env + (pram -> Closure(e, env)));
    case VarExp(str) => env(e.asInstanceOf[VarExp]);
  }

  def apply(f: Closure, x: Exp): Any = f match {
    case Closure(LambExp(cont, param, body), env) => eval(body, env + (param -> x));
  }

  def main(args: Array[String]) {
    println("Hello World!");
  }
}
