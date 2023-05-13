package com.industries.damian

import org.scalatest.funsuite.AnyFunSuite

import Main._

class Tests extends AnyFunSuite {
  test("given a paragraph, find the top three words") {
    assert(top_3_words("In a village of La Mancha, the name of which I have no desire to call to mind, there lived not long since one of those gentlemen that keep a lance in the lance-rack, an old buckler, a lean hack, and a greyhound for coursing. An olla of rather more beef than mutton, a salad on most nights, scraps on Saturdays, lentils on Fridays, and a pigeon or so extra on Sundays, made away with three-quarters of his income.") == List("a", "of", "on"))
  }

  test("given letter sequences, of mixed case, find tHe top three sequences that obey the word rules") {
    assert(top_3_words("e e e e DDD ddd DdD: ddd ddd aa aA Aa, bb cc cC e e e") == List("e", "ddd", "aa"))
  }

  test("given different variations of won't, find the top (up to three) valid versions") {
    assert(top_3_words("  //wont won't won't") == List("won't", "wont"))
  }
}