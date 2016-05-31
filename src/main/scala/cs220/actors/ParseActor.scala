package cs220.actors

import akka.actor.Actor
import akka.actor.{ActorRef, ActorLogging}

/**
 * The `ParseActor` handles the parsing of HTML documents. Its
 * primary goal is to extract out the links and words contained
 * in an HTML document.
 */
class ParseActor(pq: ActorRef) extends Actor with ActorLogging {
  log.info("ParseActor created")
  pq ! NeedPage
  val headRegex = """(?s)<head.*>.*</head>""".r
  val scriptRegex = """(?s)<script.*>.*</script>""".r
  val styleRegex = """(?s)<style.*>.*</style>""".r
  val commentRegx = """(?s)<!--.*-->""".r
  val linkRgex = """(https?://[^\"]+)""".r
  val whiteSpaceRegex = """[ \t\x0B\f\r]+""".r
  val tagRegex = """</?[^>]+>""".r

  def receive = {
    // TODO
    case ParsePage(url,html)    =>{
                                  println("ParseActor receive ParsePage Message\n" + sender)
                                  var lines = html
                                  lines = headRegex replaceAllIn(lines,"")
		                              lines = scriptRegex replaceAllIn(lines,"")
		                              lines = styleRegex replaceAllIn(lines,"")
		                              lines = commentRegx replaceAllIn(lines,"")
		                              val links = (linkRgex findAllIn lines).mkString(",")
		                              val arrayLinks = links.split(",")
		                              lines = tagRegex replaceAllIn(lines,"")
		                              lines = lines.split("\n").filter(_.length>0).mkString
		                              lines = whiteSpaceRegex replaceAllIn(lines," ")
		                              val k = lines.split(" ").tail
                                  arrayLinks.foreach(e => sender ! CheckLink(e))
                                  k.foreach(e => sender ! Word(url,e))
                                  pq ! NeedPage
                                }
    case NoPages                => {
                                  println("ParseActor NoPages receive Message\n" + sender)
                                  pq ! NeedPage
                                }
  }
}
