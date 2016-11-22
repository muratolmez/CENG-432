class IyteMutableSet {
  var simpleHashTable = new Array[IyteMutableList](97)

  def add(x: Int) {
    val index = x % 97
    if(simpleHashTable(index) == null) {
      simpleHashTable(index) = IyteMutableList()
      simpleHashTable(index).add(x)
    } else if(!simpleHashTable(index).contains(x)) {
      simpleHashTable(index).add(x)
    }
  }

  def contains(x: Int): Boolean = {
    for(i <- 0 to 96) {
      if (simpleHashTable(i) != null) {
        if(simpleHashTable(i).contains(x)) {
          return true
        }
      }
    }
    return false
  }

  override def toString(): String = {
    var result = ""
    var sum = ""
    var count = 0
    for(i <- 0 to 96) {
      if(simpleHashTable(i) != null) {
        result = simpleHashTable(i).toString()
        count = i
        if(count <= simpleHashTable.length-2) {
          sum += result + ","
        } else {
          sum += result
        }
      }
    }

    sum
  }
}

object IyteMutableSet {
  def apply(): IyteMutableSet = new IyteMutableSet()
}
