package com.leifandersen.cpslambdacalc

class Store(maxId: Int) {
  var id = 0;

  var aStore = Map[Address, Set[Closure]]();

  def aextendSingle(e: Closure): Address = {
    val addr = Address(id);
    if(aStore.contains(addr)) {
      aStore = aStore + (addr -> (aStore(addr) + e));
    } else {
      aStore += (addr -> Set(e));
    }
    id = (id + 1) % maxId;
    return addr;
  }

  def aextend(e: Set[Closure]): Address = {
    val addr = Address(id);
    if(aStore.contains(addr)) {
      for(i <- e) {
        aStore += (addr -> (aStore(addr) + i));
      }
    } else {
      aStore += (addr -> e);
    }
    id = (id + 1) % maxId;
    return addr;
  }

  def alookup(e: Address): Set[Closure] = {
    return aStore(e);
  }

  override def toString = aStore.toString

  override def equals(a: Any) = a match {
    case s: Store => aStore = s.aStore
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

