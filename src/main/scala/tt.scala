
import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
  def get(): Int
 
  def put(x: Int):Unit
}
 
class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
 
  override def get() = buf.remove(0)
 
  override def put(x: Int) = {
    buf += x
  }
}
 
class BasicIntQueue2 extends IntQueue {
  private val buf = new ArrayBuffer[Int]
 
  override def get() = buf.remove(0)
 
  override def put(x: Int) = {
    buf += 2 * x
  }
}
 
trait Doubling extends IntQueue {
  abstract override def put(x: Int) = {
    println("doubling")
    super.put(2 * x)
  }
}
 
 
trait Incrementing extends IntQueue {
  abstract override def put(x: Int) = {
    println("incrementing")
    super.put(x + 1)
  }
}
 
trait Filtering extends IntQueue {
  abstract override def put(x: Int) = {
    println("filtering")
    if (x >= 0) super.put(x)
  }
}
 
class MyQueue0 extends BasicIntQueue with Doubling
 
class MyQueue00 extends BasicIntQueue with Doubling with Filtering
 
object Main87 {
  def main(args: Array[String]) = {
    val queue = new BasicIntQueue
    queue.put(10)
    queue.put(20)
    println(queue.get())
    println(queue.get())
 
    val q = new MyQueue00
    q.put(-1)
    q.put(0)
    q.put(1)
    println(q.get())
    println(q.get())
 
    val myQueue = new MyQueue0
    myQueue.put(10)
    println(myQueue.get())
 
    val myQueue2 = (new BasicIntQueue with Filtering with Incrementing)
    myQueue2.put(-1)
    println("put end")
    myQueue2.put(0)
    println("put end")
    myQueue2.put(1)
    println("put end")
    println(myQueue2.get())
  }
}