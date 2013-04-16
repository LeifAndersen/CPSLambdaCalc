package com.leifandersen.cpslambdacalc

class Store(val maxId: Int) {
  var id = 0;

  var aStore = Map[Address, Set[Closure]]();
  for(i <- 0 to maxId) {
    val addr = Address(i);
    aStore += (addr -> Set[Closure]());
  }

  def aextendSingle(e: Closure): Address = {
    val addr = Address(id);
    aStore += (addr -> (aStore(addr) + e));
    id = (id + 1) % maxId;
    return addr;
  }

  def aextend(e: Set[Closure]): Address = {
    val addr = Address(id);
    aStore += (addr -> (aStore(addr) ++ e));
    id = (id + 1) % maxId;
    return addr;
  }

  def alookup(e: Address): Set[Closure] = {
    return aStore(e);
  }

  override def toString = aStore.toString

  override def equals(a: Any) = a match {
    case s: Store => aStore == s.aStore
    case _ => false
  }

  def size: Int = maxId;

  def copy(other: Store) {
    aStore = other.aStore;
  }
}

object Store {
  def apply(i: Int) = new Store(i)
  def copy(store: Store): Store = {
    var s = Store(store.size);
    s.copy(store);
    return s;
  }
}

