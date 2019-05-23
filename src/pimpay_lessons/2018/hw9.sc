object hw9 {
  // Че нужно юзать
  // Tuple2
  // List
  // Option
  // PartialFunction
  // Array

  // unordered hash map via bucket hash function + bucket
  // https://ru.wikipedia.org/wiki/%D0%A5%D0%B5%D1%88-%D1%82%D0%B0%D0%B1%D0%BB%D0%B8%D1%86%D0%B0
  // через Метод цепочек
  class Map[K,V] extends PartialFunction[K,V] with (K => V) {
    def apply(key:K):V = ???
    override def isDefinedAt(key: K): Boolean = ???

    def pairs:List[(K,V)] = ???
    def keys:List[K] = ???
    def values:List[V] = ???

    def mapValues[B](f: V => B):Map[K,B] = ???
    def map[B](f: (K,V) => B):Map[K,B] = ???
    def size:Int = ???
    def filter(f: V => Boolean):Map[K,V] = ???

    def get(key:K):Option[V] = ???
    def getOrElse(key:K, default: => V):V = ???

    def exists(f: V => Boolean):Boolean = ???
  }

  object Map {
    def apply[K,V](pairs:(K,V)*):Map[K,V] = ???
  }

  val map:Map[String,Int] = Map(
    "a" -> 6,
    "b" -> 7,
    "c" -> 20
  )

  map("a")
  // map.apply("a")
}

