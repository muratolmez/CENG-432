abstract class IyteImmutableSet {
  def bucket: Array[ILL]
  def add(x: Int): IyteImmutableSet
  def contains(x:Int):Boolean
}

class Node_Set(var bucket:Array[ILL]) extends IyteImmutableSet {

  override def add(x: Int): IyteImmutableSet = {
    val bucket2=bucket.clone()
    val index: Int = hashCode(x)
    if (bucket2(index) == NonValue() || !bucket2(index).contains(x))
      bucket2(index) = bucket2(index).add(x)
    return new Node_Set(bucket2)
  }

  private def hashCode(item: Int): Int = {
    (item.hashCode() % 139) % 47
  }

  override def toString: String = {
    var result: String = ""
    for (i <- 0 until bucket.length) {
      if (bucket(i) != NonValue())
        result = result + bucket(i) + ","
    }
    if (bucket.isEmpty)
      return ""
    return result.dropRight(1)
  }

  override def contains(x: Int): Boolean = {
    val index: Int = hashCode(x)
    bucket(index).contains(x)
  }
}

abstract class ILL {
  def head:Int
  def tail:ILL
  def add(x:Int):ILL
  def contains(x:Int):Boolean
}

case class NonValue() extends ILL {
  override def head: Int = throw new IllegalArgumentException("no  such an element")

  override def tail: ILL = throw new IllegalArgumentException("no  such an element")

  override def add(x: Int): ILL = new Node(x, NonValue())

  override def contains(x: Int): Boolean = false
}

class Node(override val head:Int,override val tail:ILL) extends ILL {

  override def add(x: Int): ILL = {
    new Node(x, this)
  }

  override def contains(x: Int): Boolean = {
    goToNext(x, this)
  }

  private def goToNext(item: Int, list: ILL): Boolean = {
    list match {
      case NonValue() => false
      case a =>
        if (a.head == item)
          true
        else goToNext(item, a.tail)
    }
  }

  override def toString: String = {
    next(this, "")
  }

  private def next(t: ILL, result: String): String = {
    t match {
      case NonValue() => result.dropRight(1)
      case _ => next(t.tail, t.head + "," + result)
    }
  }
}

object  ILL {
  def apply(): ILL = NonValue()
}

object IyteImmutableSet {
  def apply(): IyteImmutableSet = new Node_Set(Array.fill[ILL](139)(ILL()))
}
