package com.leifandersen.cpslambdacalc

class Store() {

  var aStore = Map[Address, Set[Closure]]();

  def aextendSingle(a: Address, e: Closure): Address = {
    if(aStore.contains(a)) {
      aStore += (a -> (aStore(a) + e));
    } else {
      aStore += (a -> Set(e));
    }
    return a;
  }

  def aextend(a: Address, e: Set[Closure]): Address = {
    if(aStore.contains(a)) {
      aStore += (a -> (aStore(a) ++ e));
    } else {
      aStore += (a -> e);
    }
    return a;
  }

  def alookup(e: Address): Set[Closure] = {
    return aStore(e);
  }

  override def toString = aStore.toString

  override def equals(a: Any) = a match {
    case s: Store => aStore == s.aStore
    case _ => false
  }

  def copy(other: Store) {
    aStore = other.aStore;
  }
}

object Store {
  def apply() = new Store()
  def copy(store: Store): Store = {
    var s = Store();
    s.copy(store);
    return s;
  }
}

