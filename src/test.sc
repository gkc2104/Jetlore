def convert(tweet: List[Char], locations: List[(Int,Int,String)]): List[Char] = {
  val result = tweet.zipWithIndex.toList.flatMap{
    x=>
      val startEntity = locations.filter(p => p._1 == x._2)
      val endEntity = locations.filter(p => p._2 == x._2)
      (startEntity, endEntity) match {
        case (null,null) => List(x._1)
        case (ls,null) =>  headMapping(ls.head._3,tweet.slice(ls.head._1,ls.head._2).toString) ++ List(x._1)
        case (null,ls) =>  List(x._1) ++  tailMapping(ls.head._3)
      }
  }.toList
  result
}

def headMapping(entityType: String, base: String): List[Char] = {
  entityType match {
    case "Entity" => val a: String = "</strong>"; a.toList
    case "Twitter Username" => {val a: String = "<a href= http://twitter.com/" +: base :+ ">"; a.toList }
    case "Link" => {val a: String = "<a href= " +: base :+ ">"; a.toList }
  }
}

def tailMapping(entityType: String): List[Char] = {
  entityType match {
    case "Entity" =>  val a: String = "</strong>"; a.toList
    case _ => List('<','/','a','>')
  }
}

val tweet: String = "Obama visited Facebook headquarters: http://bit.ly/xyz @elversatile"
val locations: List[(Int,Int,String)] = List((14,22,"Entity"),(0,5,"Entity"),(48,56,"Twitter Username"),(37,47,"Link"))
convert(tweet.toList,locations)