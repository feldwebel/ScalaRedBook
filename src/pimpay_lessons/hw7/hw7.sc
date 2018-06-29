import pimpay_lessons.hw7._

val a = MyList(1, 2, 3, 4)
val b = MyList(9, 8, 7, 6)
val c = MyList(a, b)
MyList.head(a)
MyList.dropWhile(a, (x:Int) => x < 3)
MyList.drop(a, 3)
MyList.map(a, (i:Int) => i * 2)

MyList.append(a, b)
MyList.flatten(c)
MyList.flatMap(a, (x:Int) => MyList(x, x*x))
MyList.filter(a, (x:Int) => x % 2 == 0 )