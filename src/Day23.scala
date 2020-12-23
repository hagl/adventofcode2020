import scala.collection.mutable.ArraySeq

object Day23 extends App {
  
  val lastValue = 1000000

  val a = new ArraySeq[(Int, Int)](lastValue+1)
  
  val input = List(4,1,8,9,7,6,2,3,5)
  // val input = List(3,8,9,1,2,5,4,6,7)

  Range(1, lastValue+1).foreach {
    ix => a(ix) = (ix-1, ix+1)
  }

  (lastValue :: input ::: List(input.head)).sliding(3,1).foreach {
    case List(p,x,n) => a(x) = (p,n)
  }

  a(input.head) = (lastValue, input.tail.head)
  a(input.last) = (input.init.last, 10)
  a(lastValue) = (lastValue-1, input.head)

  def step(i: Int): Int = {
    val (p, x) = a(i) 
    val (_, y) = a(x)
    val (_, z) = a(y)
    val (_, n) = a(z)
    val (_, nn) = a(n)
    
    // take out
    a(i) = (p,n)
    a(n) = (i, nn)

    var ip = i - 1 
    if (ip == 0) {
      ip = lastValue
    }
    while (ip == x || ip == y || ip == z) {
      ip -= 1
      if (ip == 0) ip = lastValue
    }
    val (p1, n1) = a(ip)
    val (_, nn1) = a(n1)

    a(x) = (ip, y)
    a(z) = (y, n1)
    a(ip) = (p1, x)
    a(n1) = (z, nn1)

    n
  }

  var ix = input.head

  Range(1,10000001).foreach {
    _ => {
      ix = step(ix)
    }
  }

  def toList(start: Int, count: Int, acc: List[Int] = List()): List[Int] = {
    if (count == 0) acc.reverse else toList(a(start)._2, count - 1, start :: acc)
  }

  val resultList = toList(1,9)
  println(resultList)
  println(resultList(1).toLong * resultList(2).toLong)

}

