/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class ZScoreNormalizerSpec extends AnyFunSuite {

  test("case 1: 1 normalization") {
    val zScoreNormalizer = new ZScoreNormalizer
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val normalizedTest = zScoreNormalizer.normalize(0, xs)
    val normalizedTruth = List(-1.224744871391589, 0.0, 1.224744871391589)
    assert(normalizedTest == normalizedTruth)
  }

  test("case 2: 2 normalizations") {
    val zScoreNormalizer = new ZScoreNormalizer

    val xs: List[Double] = List(0.0, 5.0, 10.0)
    zScoreNormalizer.normalize(0, xs)

    val newXs: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = zScoreNormalizer.normalize(0, newXs)
    val normalizedTruth2 = List(2.449489742783178, 3.674234614174767)
    assert(normalizedTest2 == normalizedTruth2)
  }

  test("case 3: 1 normalization, 2 vectors") {
    val zScoreNormalizer = new ZScoreNormalizer

    val xs1: List[Double] = List(0.0, 5.0, 10.0)
    zScoreNormalizer.normalize(0, xs1)

    val xs2: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = zScoreNormalizer.normalize(1, xs2)
    val normalizedTruth2 = List(-1.0, 1.0)
    assert(normalizedTest2 == normalizedTruth2)
  }

}