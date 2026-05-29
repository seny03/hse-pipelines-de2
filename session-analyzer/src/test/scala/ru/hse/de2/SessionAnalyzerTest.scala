package ru.hse.de2

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SessionAnalyzerTest extends AnyFunSuite with Matchers {

  test("parseDate parses DD.MM.YYYY_HH:MM:SS format") {
    SessionAnalyzer.parseDate("15.04.2024_10:30:00") shouldBe Some("2024-04-15")
  }

  test("parseDate parses DayName,_D_MonName_YYYY_HH:MM:SS_+0300 format") {
    SessionAnalyzer.parseDate("Monday,_1_Jan_2024_10:00:00_+0300") shouldBe Some("2024-01-01")
  }

  test("parseDate pads single-digit day with zero") {
    SessionAnalyzer.parseDate("Friday,_5_Mar_2024_10:00:00_+0300") shouldBe Some("2024-03-05")
  }

  test("parseDate returns None for unknown format") {
    SessionAnalyzer.parseDate("not a date") shouldBe None
  }

  test("parseDate returns None for unknown month") {
    SessionAnalyzer.parseDate("Monday,_1_Xxx_2024_10:00:00_+0300") shouldBe None
  }

  test("isDocId accepts valid doc id") {
    SessionAnalyzer.isDocId("ACC_45616") shouldBe true
    SessionAnalyzer.isDocId("LAW_221940") shouldBe true
    SessionAnalyzer.isDocId("PGU_13") shouldBe true
  }

  test("isDocId rejects strings without base prefix") {
    SessionAnalyzer.isDocId("123") shouldBe false
    SessionAnalyzer.isDocId("abc_123") shouldBe false
  }

  test("isDocId rejects event-like strings") {
    SessionAnalyzer.isDocId("DOC_OPEN") shouldBe false
    SessionAnalyzer.isDocId("CARD_SEARCH_END") shouldBe false
  }

  test("isDocId rejects timestamps") {
    SessionAnalyzer.isDocId("15.04.2024_10:30:00") shouldBe false
  }

  test("parseSession extracts CARD_SEARCH results") {
    val content =
      """SESSION_START 15.04.2024_10:00:00
        |CARD_SEARCH_START 15.04.2024_10:01:00
        |$key1 value1
        |$key2 value2
        |CARD_SEARCH_END 15.04.2024_10:02:00
        |card1 ACC_45616 LAW_1 PGU_13
        |SESSION_END 15.04.2024_10:05:00
        |""".stripMargin

    val (cardSearchResults, _) = SessionAnalyzer.parseSession(content)

    cardSearchResults should have size 1
    cardSearchResults.head should contain theSameElementsAs Seq("ACC_45616", "LAW_1", "PGU_13")
  }

  test("parseSession counts multiple CARD_SEARCH in one session separately") {
    val content =
      """SESSION_START 15.04.2024_10:00:00
        |CARD_SEARCH_START 15.04.2024_10:01:00
        |$key1 value1
        |CARD_SEARCH_END 15.04.2024_10:02:00
        |card1 ACC_45616 LAW_1
        |CARD_SEARCH_START 15.04.2024_10:03:00
        |$key1 value2
        |CARD_SEARCH_END 15.04.2024_10:04:00
        |card2 ACC_45616 PGU_13
        |SESSION_END 15.04.2024_10:05:00
        |""".stripMargin

    val (cardSearchResults, _) = SessionAnalyzer.parseSession(content)

    cardSearchResults should have size 2

    val acc45616Searches = cardSearchResults.count(_.contains("ACC_45616"))
    acc45616Searches shouldBe 2
  }

  test("parseSession ignores non-doc-id tokens in CARD_SEARCH results") {
    val content =
      """CARD_SEARCH_START 15.04.2024_10:01:00
        |$key value
        |CARD_SEARCH_END 15.04.2024_10:02:00
        |card1 ACC_45616 something_bad
        |""".stripMargin

    val (cardSearchResults, _) = SessionAnalyzer.parseSession(content)

    cardSearchResults.head should contain ("ACC_45616")
    cardSearchResults.head should not contain "something_bad"
  }

  test("parseSession counts DOC_OPEN linked to QS where doc was found") {
    val content =
      """SESSION_START 15.04.2024_10:00:00
        |QS 15.04.2024_10:01:00 some query text
        |q1 ACC_45616 LAW_1
        |DOC_OPEN 15.04.2024_10:02:00 q1 ACC_45616
        |SESSION_END 15.04.2024_10:05:00
        |""".stripMargin

    val (_, qsDocOpens) = SessionAnalyzer.parseSession(content)

    qsDocOpens should contain (("2024-04-15", "ACC_45616"))
    qsDocOpens should have size 1
  }

  test("parseSession ignores DOC_OPEN if doc not in QS results") {
    val content =
      """QS 15.04.2024_10:01:00 query
        |q1 LAW_1 PGU_13
        |DOC_OPEN 15.04.2024_10:02:00 q1 ACC_45616
        |""".stripMargin

    val (_, qsDocOpens) = SessionAnalyzer.parseSession(content)

    qsDocOpens shouldBe empty
  }

  test("parseSession ignores DOC_OPEN if searchId is unknown") {
    val content =
      """QS 15.04.2024_10:01:00 query
        |q1 ACC_45616
        |DOC_OPEN 15.04.2024_10:02:00 unknown_search ACC_45616
        |""".stripMargin

    val (_, qsDocOpens) = SessionAnalyzer.parseSession(content)

    qsDocOpens shouldBe empty
  }

  test("parseSession uses DOC_OPEN date, not QS date") {
    val content =
      """QS 15.04.2024_23:59:00 query
        |q1 ACC_45616
        |DOC_OPEN 16.04.2024_00:01:00 q1 ACC_45616
        |""".stripMargin

    val (_, qsDocOpens) = SessionAnalyzer.parseSession(content)

    qsDocOpens.head._1 shouldBe "2024-04-16"
  }

  test("parseSession does not confuse CARD_SEARCH and QS DOC_OPENs") {
    val content =
      """CARD_SEARCH_START 15.04.2024_10:01:00
        |$key value
        |CARD_SEARCH_END 15.04.2024_10:02:00
        |card1 ACC_45616
        |DOC_OPEN 15.04.2024_10:03:00 card1 ACC_45616
        |""".stripMargin

    val (cardSearchResults, qsDocOpens) = SessionAnalyzer.parseSession(content)

    cardSearchResults should have size 1
    qsDocOpens shouldBe empty
  }

  test("parseSession handles empty content") {
    val (cards, opens) = SessionAnalyzer.parseSession("")
    cards shouldBe empty
    opens shouldBe empty
  }

  test("parseSession handles malformed DOC_OPEN line") {
    val content =
      """DOC_OPEN 15.04.2024_10:00:00 q1
        |""".stripMargin

    val (cards, opens) = SessionAnalyzer.parseSession(content)
    cards shouldBe empty
    opens shouldBe empty
  }

  test("parseSession handles CARD_SEARCH_START without CARD_SEARCH_END") {
    val content =
      """CARD_SEARCH_START 15.04.2024_10:00:00
        |$key value
        |""".stripMargin

    val (cards, opens) = SessionAnalyzer.parseSession(content)
    cards shouldBe empty
    opens shouldBe empty
  }
}
