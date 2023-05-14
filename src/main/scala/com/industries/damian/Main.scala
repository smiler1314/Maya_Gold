package com.industries.damian

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
}