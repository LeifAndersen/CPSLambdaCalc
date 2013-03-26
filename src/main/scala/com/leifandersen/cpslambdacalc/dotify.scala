package com.leifandersen.cpslambdacalc

object Dotify {
  def dotify(in: Map[State,Set[State]]): String = {
    var i = 1;
    var stateStrings = Map[State,String]();

    def dotifyExp(e: Exp): String = e match {
      case ApplyExp(prog, arg) => "Apply Expression";
      case HaltExp() => "Halt";
      case LambExp(param, body) => "Lambda Expression";
      case VarExp(value) => "Variable: " + value;
    }

    def dotifyStateInline(state: State): String = state match {
      case EvalState(e, env) => "Eval State";
      case ApplyState(f, x) => "Apply State";
      case HaltState() => "Halt State";
    }

    def dotifyState(state: State): String = state match {
      case EvalState(e, env) => "s" + i + "[label=\"Eval State:" + dotifyExp(e) + "\"];\n";
      case ApplyState(f, x) => "s" + i + "[label=\"Apply State:" + dotifyStateInline(f) + "\"];\n";
      case HaltState() => "s" + i + "[label=\"Halt State\"];\n";
    }
//    var text = "digraph G {\n" + in.foldLeft("")((x, y) => { i += 1; x + dotifyState(y) }) + "}\n";
    var text = "digraph G {\n";
    for(state <- in.keys) {
      text += dotifyState(state);
      stateStrings += (state -> ("s" + i));
      i += 1;
    }
    for(state <- in.keys; step <- in(state)) {
      text += stateStrings(state) + " -> " + stateStrings(step) + ";\n";
    }
    text += "}\n";
    return text;
  }
}
