package com.leifandersen.cpslambdacalc

object Dotify {
  def dotify(in: Map[State,Set[State]]): String = {
    var i = 1;
    var stateStrings = Map[State,String]();

    def dotifyExp(e: Exp): String = e match {
      case ApplyExp(prog, arg) => {
        var tmp = "(" + dotifyExp(prog);
        for (i <- arg) tmp += " " + dotifyExp(i);
        tmp += ")";
        return tmp;
      }
      case HaltExp() => "'Halt";
      case LambExp(param, body) => {
        var tmp = "(λ ( ";
        for(i <- param) tmp += dotifyExp(i) + " ";
        tmp += ") " + dotifyExp(body) + ")";
        return tmp;
      }
      case VarExp(value) => value;
    }

    def dotifyStateInline(state: State): String = state match {
      case EvalState(e, env, s) => dotifyExp(e);
      case ApplyState(f, x, s) => {
        var tmp = "(" + dotifyStateInline(f);
        for (i <- x) tmp += " " + dotifyStateInline(i);
        tmp += ")";
        return tmp;
      }
      case HaltState() => "Halt State";
    }

    def dotifyState(state: State): String = state match {
      case EvalState(e, env, s) => "s" + i + "[label=\"Eval State:\\n" + dotifyStateInline(state) + "\"];\n";
      case ApplyState(f, x, s) => "s" + i + "[label=\"Apply State:" + dotifyStateInline(state) + "\"];\n";
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
