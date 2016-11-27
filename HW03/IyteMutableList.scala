class IyteMutableList{


	class Node(var data : Int, var next: Node)
	private var head:Node = null
	private var tail:Node = null


	def add(data :Int):Unit={

		if(head != null){

			tail.next = new Node(data,null)
			tail = tail.next

		}

		else{

			head = new Node(data,null)
			tail=head
		}
	}

override def toString() : String = {

	var result:String = ""
	var flag = head
	if(flag == null)
		return ""

	while(flag != null){
		result += flag.data
		result += ","
		flag = flag.next
	}
	return result.substring(0,result.length-1)
}


	

}

object IyteMutableList {
	
	def apply() : IyteMutableList = new IyteMutableList()
}
