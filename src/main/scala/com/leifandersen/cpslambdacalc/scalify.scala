package com.leifandersen.cpslambdacalc

object Scalify {

  var buff = "";

  def scalify(in: String): Exp = {
    buff = in;
    return null;
  }

  def peek(): Char = {
    return buff.charAt(0);
  }

  def next(): Char = {
    val tmp = buff.charAt(0);
    buff = buff.tail
    return tmp;
  }

  def eat(in: Char): Char = {
    val tmp = next();
    if(next == tmp) {
      return tmp;
    } else {
      return tmp;
    }
  }

}

