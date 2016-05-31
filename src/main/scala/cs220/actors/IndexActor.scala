package cs220.actors

import scalikejdbc._
import akka.actor.{Actor, ActorLogging}

/**
 * The `IndexActor` is responsible for communicating with the
 * database to index a word.
 */
class IndexActor extends Actor with ActorLogging {

  log.info("IndexActor created")

  def receive = {
    // TODO

    case CheckPage(url,html)   => {
                                    println("IndexActor receive CheckPage Message\n" + sender)
                                    if(!getLinks.contains(url)){
                                    sql"""insert into documents(url)""".update.apply()
                                    sender ! ParsePage(url,html)
                                  }
                                }
   case CheckLink(url)           => {println("IndexActor receive CheckLink Message\n" + sender)
                                    if(!getLinks.contains(url)){
                                    sql"""insert into documents(urls)""".update.apply()
                                    sender ! QueueLink(url)
                                  }
                                }

  case Word(u,w)            => {
                                    println("IndexActor receive word Message\n" + sender)
                                    sql"""insert into words(w)""".update.apply()
                                    sql"""insert into index(wordid,docid)
                                    select words.wordid,documents.docid
                                    from words W,documents D
                                    where W.word = 'w'
                                    and D.url = 'u' """.update.apply()
                                  }
  }

  ///////////////////////////////////////////////////////////////////
  // The code below is a starting point for your queries/updates to
  // the database. We have provided the database creation SQL for
  // you. You will not need to add any additional tables. Your goal
  // is to populate it with data you have received from parsed HTML
  // documents. We strongly suggest that you implement each of your
  // queries as individual methods in this class, where each method
  // corresponds to some query that is useful in building the index.
  ///////////////////////////////////////////////////////////////////

  // Necessary setup for connecting to the H2 database:
  Class.forName("org.h2.Driver")
  ConnectionPool.singleton("jdbc:h2:./indexer", "sa", "")
  implicit val session = AutoSession

  // Create the database when this object is referenced.
  createDatabase

  def createDatabase: Unit = {
    sql"""
      drop table words if exists;
      drop table documents if exists;
      drop table index if exists;
    """.update.apply()

    // Create the tables if they do not already exist:
    sql"""
    create table if not exists words (
      wordid int auto_increment,
      word varchar(50),
      primary key (wordid)
    );
    """.update.apply()

    sql"""
    create table if not exists documents (
      docid int auto_increment,
      url varchar(1024),
      primary key (docid)
    );
    """.update.apply()

    sql"""
    create table if not exists index (
      wordid int,
      docid int,
      foreign key (wordid) references words (wordid) on delete cascade,
      foreign key (docid) references documents (docid) on delete cascade
    );
    """.update.apply()
  }


  def getLinks() : List[String] = {
   val links = sql"""select url from documents""".map(
     rs => rs.string("url")).list.apply()
     links
    }
}
