/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class ZScoreNormalizerSpec extends AnyFunSuite {

  test("case 1: 2 normalizations") {
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val zScoreNormalizer = new ZScoreNormalizer
    val normalizedTest = zScoreNormalizer.normalize(xs)
    val normalizedTruth = List(-1.224744871391589, 0.0, 1.224744871391589)
    assert(normalizedTest == normalizedTruth)
    val newXs: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = zScoreNormalizer.normalize(newXs)
    val normalizedTruth2 = List(2.449489742783178, 3.674234614174767)
    assert(normalizedTest2 == normalizedTruth2)
  }

}