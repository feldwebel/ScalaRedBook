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

c.foldRight(List[Int]())((item: Int, acc: List[Int]) => (item * 2) :: acc)


val rows = 5
val cols = 5
val a1 = Array.ofDim[Char](rows, cols)

a1(0) = "hello".toCharArray
a1(1) = "Dolly".toCharArray
a1(2) = "well,".toCharArray
a1(3) = "hello".toCharArray
a1(4) = "Dolly".toCharArray

val sb = StringBuilder.newBuilder

for { i <- 0 until rows} sb.append(a1(i).mkString + System.lineSeparator())

println(sb.toString())

a1(0)(0)
a1(1)(1)


//print("kuku", "\033{6;8H", "dudu")

print(raw"\u001b[31mHello\u001b[0mWorld")
