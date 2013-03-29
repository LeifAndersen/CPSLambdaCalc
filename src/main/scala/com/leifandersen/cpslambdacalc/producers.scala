package com.leifandersen.cpslambdacalc

import scala.actors.Actor
import scala.actors.Actor._

case object Next
case object Stop

class SetStateProducer(n: Set[State]) extends Producer[Map[State, Set[State]]] {
  def produceValues = traverse(n);
  def traverse(n: Set[State]) {
    if(n != null) {
      for(i <- n) {
        var next = Analysis.astep(i)
        produce(Map(i -> next))
      }
    }
  }
}

class StateProducer(n: State) extends Producer[Set[State]] {
  def produceValues = traverse(n);
  def traverse(n: State) {
    if(n != null) {
      produce(Analysis.astep(n))
    }
  }
}

class EvalProducer(n: State) extends Producer[Set[Closure]] {
  def produceValues = traverse(n);
  def traverse(n: State) {
    if(n != null) {
      produce(Analysis.aevalState(n))
    }
  }
}

abstract class Producer[T] {
  private val Undefined = new Object
  protected def produceValues: Any
  protected def produce(x: T) {
    coordinator ! Some(x)
    receive { case Next => }
  }

  private val producer: Actor = actor {
    receive {
      case Next =>
        produceValues
        coordinator ! None
    }
  }

  private val coordinator: Actor = actor {
    loop {
      react {
        case Next =>
          producer ! Next
          reply {
            receive { case x: Option[_] => x}
          }
        case Stop => exit('Stop)
      }
    }
  }

  def iterator = new Iterator[T] {
    private var current: Any = Undefined
    private def lookAhead = {
      if (current == Undefined) current = coordinator !? Next
      current
    }

    def hasNext: Boolean = lookAhead match {
      case Some(x) => true
      case None => { coordinator ! Stop; false }
    }

    def next: T = lookAhead match {
      case Some(x) => current = Undefined; x.asInstanceOf[T]
    }
  }
}

