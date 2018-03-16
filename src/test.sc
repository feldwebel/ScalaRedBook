val a = List(1, 2, 3)
val b = List(4, 5, 6)
val c = List(10, 11, 12, 13)

for (x <-a; y <-b) yield {x::y::Nil}


val x = Option(14)
var y = Option(88)

x flatMap (aa => y map(bb => aa + bb))
x map (aa => y map(bb => aa + bb))

for (aa <-x; bb <-y) yield { aa + bb}

a.zipAll(c, 0, 0)

