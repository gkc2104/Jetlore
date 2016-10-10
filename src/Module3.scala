/**
  * Created by KAMAL on 07-10-2016.
  */
object Module3 {
  def main(args: Array[String]){
    val tweet: String = "Obama visited Facebook headquarters: http://bit.ly/xyz @elversatile"
    val locations: List[(Int,Int,String)] = List((14,21,"Entity"),(0,4,"Entity"),(56,67,"Twitter Username"),(37,54,"Link"))
    val ans = convert(tweet.toList,locations).mkString
    println(ans)
  }

  /**
    *
    * @param tweet     : Input Tweet as a List of Charactes
    * @param locations : Mapping for what boundaries of each entities
    * @return          : Converted ouput in HTML format
    */
  def convert(tweet: List[Char], locations: List[(Int,Int,String)]): List[Char] = {
    val result = tweet.zipWithIndex.toList.flatMap{
      x=>
        val startEntity = locations.filter(p => p._1 == x._2)
        val endEntity = locations.filter(p => p._2 == x._2)
        (startEntity, endEntity) match {
          case (List(a),_) => headMapping(a._3,tweet.slice(a._1,a._2).mkString) ++ List(x._1)
          case (_,List(a)) =>  List(x._1) ++  tailMapping(a._3)
          case (_,_) => List(x._1)
        }
    }.toList
    result
  }

  /**Adds the head tag
    *
    * @param entityType The type of tag it is
    * @param base       The value that might habe to be stored if it is a link
    * @return           Returns a modified version of the entity
    */
  def headMapping(entityType: String, base: String): List[Char] = {
    entityType match {
      case "Entity" => "<strong>".toList
      case "Twitter Username" => ("<a href= http://twitter.com/" + base + ">").toList
      case "Link" => ("<a href= " + base + ">").toList
      case _ => "".toList
    }

  }

  /**
    * Adds the tail tag
    *
    * @param entityType The type of tag it is
    * @return
    */
  def tailMapping(entityType: String): List[Char] = {
    entityType match {
      case "Entity" =>  val a: String = "</strong>"; a.toList
      case _ => List('<','/','a','>')
    }
  }
}

