package com.leifandersen.cpslambdacalc

abstract class Exp { }

abstract class CExp extends Exp { }

case class ApplyExp(prog: AExp, arg: List[AExp]) extends CExp { }

abstract class AExp() extends Exp { }

case class LambExp(param: List[VarExp], body: CExp) extends AExp { }

case class VarExp(value: String) extends AExp { }

case class HaltExp() extends AExp { }


case class Closure(e: Exp, env: Map[VarExp, Address]) { }


case class Address(address: Exp) { }


abstract class State { }

case class EvalState(e: Exp, env: Map[VarExp, Address], store: Store) extends State { }

case class ApplyState(f: EvalState, x: List[EvalState], store: Store) extends State { }

case class HaltState() extends State { }
