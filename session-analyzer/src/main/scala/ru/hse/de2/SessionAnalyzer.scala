package ru.hse.de2

import org.apache.spark.SparkContext
import org.apache.spark.sql.SparkSession
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object SessionAnalyzer {

  private val log = LoggerFactory.getLogger(getClass)

  private val DatePattern1 = """(\d{2})\.(\d{2})\.(\d{4})_.*""".r
  private val DatePattern2 = """\w+,_(\d+)_(\w+)_(\d{4})_.*""".r

  private val DocIdPattern = """[A-Z]+_\d+""".r

  private val monthMap = Map(
    "Jan" -> "01", "Feb" -> "02", "Mar" -> "03", "Apr" -> "04",
    "May" -> "05", "Jun" -> "06", "Jul" -> "07", "Aug" -> "08",
    "Sep" -> "09", "Oct" -> "10", "Nov" -> "11", "Dec" -> "12"
  )

  def parseDate(ts: String): Option[String] = ts match {
    case DatePattern1(d, m, y) =>
      Some(s"$y-$m-$d")

    case DatePattern2(d, mon, y) =>
      monthMap.get(mon).map { m =>
        s"$y-$m-${"%02d".format(d.toInt)}"
      }

    case _ =>
      None
  }

  def isDocId(value: String): Boolean =
    DocIdPattern.matches(value)

  /**
   * Parses a single session file and returns:
   *
   *   - Seq[Seq[String]]
   *       document id lists for each CARD_SEARCH result
   *
   *   - Seq[(String, String)]
   *       (openDate, docId) for DOC_OPEN events where the document was found
   *       in the corresponding QS result list
   */
  def parseSession(content: String): (Seq[Seq[String]], Seq[(String, String)]) = {
    val lines = content
      .split("\n")
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList

    val qsSearches = mutable.Map.empty[String, Set[String]]

    val cardSearchResults = mutable.ArrayBuffer.empty[Seq[String]]
    val qsDocOpens = mutable.ArrayBuffer.empty[(String, String)]

    var i = 0

    while (i < lines.length) {
      val line = lines(i)

      if (line.startsWith("QS ")) {
        val parts = line.split("\\s+", 3)
        if (parts.length < 2 || parseDate(parts(1)).isEmpty)
          log.warn(s"Unrecognized QS timestamp: ${parts.lift(1).getOrElse("")}")

        i += 1

        if (i < lines.length) {
          val tokens = lines(i).split("\\s+").filter(_.nonEmpty)

          if (tokens.length > 1) {
            val searchId = tokens(0)
            val docs = tokens.tail.filter(isDocId).toSet

            if (docs.nonEmpty) {
              qsSearches(searchId) = docs
            }
          }
        }

      } else if (line.startsWith("CARD_SEARCH_START")) {
        /*
         * Expected structure:
         *
         * CARD_SEARCH_START ...
         * <param lines>
         * CARD_SEARCH_END ...
         * <searchId> <docId1> <docId2> ...
         */
        i += 1

        while (i < lines.length && !lines(i).startsWith("CARD_SEARCH_END")) {
          i += 1
        }

        if (i < lines.length && lines(i).startsWith("CARD_SEARCH_END")) {
          i += 1

          if (i < lines.length) {
            val tokens = lines(i).split("\\s+").filter(_.nonEmpty)

            if (tokens.length > 1) {
              val docs = tokens.tail.filter(isDocId).toSeq

              if (docs.nonEmpty) {
                cardSearchResults += docs
              }
            }
          }
        }

      } else if (line.startsWith("DOC_OPEN ")) {
        /*
         * Expected structure:
         *
         * DOC_OPEN <timestamp> <searchId> <docId>
         */
        val parts = line.split("\\s+").filter(_.nonEmpty)

        if (parts.length >= 4) {
          val openDate = parseDate(parts(1))
          val searchId = parts(2)
          val docId = parts(3)

          for {
            date <- openDate
            foundDocs <- qsSearches.get(searchId)
            if foundDocs.contains(docId)
          } {
            qsDocOpens += ((date, docId))
          }
        }
      }

      i += 1
    }

    (cardSearchResults.toSeq, qsDocOpens.toSeq)
  }

  def main(args: Array[String]): Unit = {
    val dataPath = if (args.nonEmpty) args(0) else "Сессии"
    val outputPath = if (args.length > 1) args(1) else "output"

    val spark = SparkSession.builder()
      .appName("ConsultantPlusSessionAnalyzer")
      .getOrCreate()

    val sc: SparkContext = spark.sparkContext

    val parsed = sc
      .wholeTextFiles(dataPath)
      .flatMap { case (path, content) =>
        Try(parseSession(content)) match {
          case Success(result) => Some(result)
          case Failure(e) =>
            log.warn(s"Failed to parse session file $path: ${e.getMessage}")
            None
        }
      }
      .cache()

    parsed
      .flatMap { case (cardSearchResults, _) => cardSearchResults }
      .map(_.mkString(","))
      .saveAsTextFile(s"$outputPath/parsed_card_search_results")
    
    parsed
      .flatMap { case (_, docOpens) => docOpens }
      .map { case (date, docId) => s"$date\t$docId" }
      .saveAsTextFile(s"$outputPath/parsed_qs_doc_opens")


    // Task 1: Count CARD_SEARCH result lists that contain ACC_45616.
    val cardSearchCount = parsed
      .flatMap { case (cardSearchResults, _) => cardSearchResults }
      .filter(_.contains("ACC_45616"))
      .count()

    println(s"Task 1: ACC_45616 in CARD_SEARCH results: $cardSearchCount")

    // Task 2: Count DOC_OPEN events per (openDate, docId) for documents found via QS.
    val qsDocOpenCounts = parsed
      .flatMap { case (_, docOpens) => docOpens }
      .map(pair => (pair, 1))
      .reduceByKey(_ + _)
      .map {
        case ((date, docId), count) =>
          s"$date\t$docId\t$count"
      }
      .sortBy(identity)

    println("Task 2: DOC_OPEN counts per (date, doc) via QS, first 20 rows")
    qsDocOpenCounts.take(20).foreach(println)

    qsDocOpenCounts.saveAsTextFile(s"$outputPath/task2_qs_doc_opens_per_day")

    sc.parallelize(
      Seq(s"ACC_45616 appeared in CARD_SEARCH results: $cardSearchCount times")
    ).saveAsTextFile(s"$outputPath/task1_card_search_acc45616")

    spark.stop()
  }
}
