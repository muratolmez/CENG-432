sealed trait IyteImmutableList{
	
	def add(data : Int) :IyteImmutableList
}

case object Nil extends IyteImmutableList{
	
	override def add(data : Int) : IyteImmutableList = Cons(data,Nil)
	
	override def toString = ""
}

case class Cons(head: Int, tail: IyteImmutableList) extends IyteImmutableList{
	
	override def add(data : Int) : IyteImmutableList =  Cons(data,this)
	
	override def toString : String= 

	{
		
		def go(l : IyteImmutableList) : String= l match 

		{
			
			case Nil => Nil.toString
			case Cons(data,xs) => data.toString + "," + go(xs)
		
		}
		 

		 val value=go(this)
		 value.substring(0,value.length-1)
	}
}

object IyteImmutableList
{
	def apply() = Nil
}
