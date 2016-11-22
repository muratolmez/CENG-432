class IyteMutableList {
  case class Node(value:Int, var next : Node)
  private var head: Node = null
  private var tail: Node = null



  def add(x: Int) {
    if(head == null){
      head = Node(x, null)
      tail = head
    } else {
      tail.next = Node(x, null)
      tail = tail.next
    }
  }
  override def toString(): String = {
    var temp = head
    var result = ""
    while(temp != null){
      if(temp.next != null) {
        result += temp.value + ","
        temp = temp.next
      } else {
        result += temp.value
        temp = temp.next
      }
    }
    result
  }

  def contains(x: Int): Boolean = {
    var list = head
    while(list != null) {
      if(list.value == x) {
        return true
      }
      list = list.next
    }
    return false
  }
}

object IyteMutableList {
  def apply() = new IyteMutableList()
}
