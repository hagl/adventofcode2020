import scala.collection.mutable.ArraySeq

object Day23 extends App {
  
  val lastValue = 1000000

  val a = new ArraySeq[Int](lastValue+1)
  
  val input = List(4,1,8,9,7,6,2,3,5)
  // val input = List(3,8,9,1,2,5,4,6,7)

  Range(1, lastValue+1).foreach {
    ix => a(ix) = ix+1
  }

  (input ::: List(input.head)).sliding(2,1).foreach {
    case List(x,n) => a(x) = n
  }

  a(input.last) = 10
  a(lastValue) = input.head

  def step(i: Int): Int = {
    val x = a(i) 
    val y = a(x)
    val z = a(y)
    val n = a(z)
    val nn = a(n)
    
    // take out
    a(i) = n
    a(n) = nn

    var ip = i - 1 
    if (ip == 0) {
      ip = lastValue
    }
    while (ip == x || ip == y || ip == z) {
      ip -= 1
      if (ip == 0) ip = lastValue
    }
    val n1 = a(ip)
    val nn1 = a(n1)

    a(x) = y
    a(z) = n1
    a(ip) = x
    a(n1) = nn1

    n
  }

  var ix = input.head

  Range(1,10000001).foreach {
    _ => {
      ix = step(ix)
    }
  }

  def toList(start: Int, count: Int, acc: List[Int] = List()): List[Int] = {
    if (count == 0) acc.reverse else toList(a(start), count - 1, start :: acc)
  }

  val resultList = toList(1,9)
  println(resultList)
  println(resultList(1).toLong * resultList(2).toLong)

}
