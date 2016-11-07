object Simple extends App {
	val array = Array(10,25,30)
	array.foreach(n=> if(n%2==0) println(n*2) else println(n*3))	
}
