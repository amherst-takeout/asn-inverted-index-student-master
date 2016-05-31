package cs220.actors

import scala.collection.mutable.Queue
import akka.actor.Actor
import akka.actor.{ActorRef, ActorLogging}

/**
 * The `ParseQueueActor` is responsible for maintaining a queue of pages
 * to be parsed by the `ParseActor`s. It is responsible for determining
 * if a page has already been parsed. If so, it should not parse it again.
 */
class ParseQueueActor(indexer: ActorRef) extends Actor with ActorLogging {
  log.info("ParseQueueActor created")
  var linkQueue: Option[ActorRef] = None
  val queue = Queue[ParsePage]()

  def receive = {
    case Page(url, html) => {
      println("ParseQueueActor receive Page Messaged\n" + sender)

      if (linkQueue == None)
        linkQueue = Some(sender)
        println("sending checkpage to indexer\n" + linkQueue.get)
        indexer ! CheckPage(url,html)
      }

    case ParsePage(url,html) => {
                                println("ParseQueueActor receive ParsePage Message\n" + sender)
                                  queue.enqueue(ParsePage(url,html))
                                }
    case NeedPage            => {
                                    println("ParseQueueActor receive NeedPage Message\n" + sender)
                                    if(!queue.isEmpty)
                                    sender ! queue.dequeue
                                    else
                                    sender ! NoPages
                                  }

    case Link(url)           => {
      println("ParseQueueActor receive Link Message\n" + sender)
      indexer ! CheckLink(url)
    }
    case Word(url,word)      => {
      println("ParseQueueActor receive word Message\n" + sender)
      indexer ! (Word(url,word))
    }
    case QueueLink(url)      =>
    {
      println("ParseQueueActor receive QueueLink Message\n" + sender)
      linkQueue.get ! (QueueLink(url))
    }
    // MORE TO BE DONE HERE...
  }
}
