package com.leifandersen.cpslambdacalc

import scala.actors.Actor
import scala.actors.Actor._

case object Next
case object Stop

class StateProducer(n: State) extends Producer[State] {
  def produceValues = traverse(n);
  def traverse(n: State) {
    if(n != null) {
      for(i <- Analysis.astep(n)) {
        produce(n)
      }
    }
  }
}

abstract class Producer[T] {
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
}

