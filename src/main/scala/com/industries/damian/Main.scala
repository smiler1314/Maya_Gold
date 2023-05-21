package com.industries.damian

import scala.collection.immutable.Map

object Main {
  def top_3_words(str: String): List[String] = {
    str.split("[^a-zA-Z0-9']")
      .withFilter(word => word != "")
      .map(word => word.toLowerCase)
      .foldLeft(Map[String, Int]()) { (acc, word) =>
        acc.get(word) match
          case Some(freq) => acc.updated(word, freq + 1)
          case None => acc.updated(word, 1)
      }.toList
      .sortWith((entry1, entry2) => entry1._2 > entry2._2)
      .take(3)
      .map(entry => entry._1)
  }

  def top_3_words_mkII(str: String): List[String] = {
    case class WordAndFreq(word: String, freq: Int)
    case class TopThreeWords(one: WordAndFreq, two: WordAndFreq, three: WordAndFreq) {
      def contains(word: String): Boolean = {
        if (one.word == word ||
          two.word == word ||
          three.word == word) true
        else false
      }

      private def reindex: TopThreeWords = {
        val ordered: IndexedSeq[WordAndFreq] = IndexedSeq(one, two, three)
          .sortWith((wf1, wf2) => wf1.freq > wf2.freq)
        TopThreeWords(ordered(0), ordered(1), ordered(2))
      }

      def updated(word: String, wordFreqMap: Map[String, Int]): TopThreeWords = {
        val freq = wordFreqMap.getOrElse(word, 0)
        (one, two, three) match {
          case (wf, _, _) if wf.word == word => TopThreeWords(WordAndFreq(wf.word, one.freq + 1), two, three)
          case (_, wf, _) if wf.word == word => TopThreeWords(one, WordAndFreq(wf.word, two.freq + 1), three)
          case (_, _, wf) if wf.word == word => TopThreeWords(one, two, WordAndFreq(wf.word, three.freq + 1))
          case (_, _, _) if freq > three.freq => TopThreeWords(one, two, WordAndFreq(word, freq))
          case _ => TopThreeWords(one, two, three)
        }
      }.reindex
    }
    case class Acc(wordFreqMap: Map[String, Int], top3MostFreqWords: TopThreeWords)

    val noWord = WordAndFreq("", 0)

    val top3w = str.split("[^a-zA-Z0-9']")
      .withFilter(word => word != "")
      .map(word => word.toLowerCase)
      .foldLeft(Acc(Map[String, Int](), TopThreeWords(noWord, noWord, noWord))) { (acc, word) =>
        val map: Map[String, Int] = acc.wordFreqMap
        val updatedMap = map.updated(word, map.getOrElse(word, 0) + 1)
        val top3: TopThreeWords = acc.top3MostFreqWords
        val updatedTop3 = top3.updated(word, updatedMap)
        Acc(updatedMap, updatedTop3)
      }
      .top3MostFreqWords

    List(top3w.one.word, top3w.two.word, top3w.three.word)
      .filter(w => w != "")
  }
}