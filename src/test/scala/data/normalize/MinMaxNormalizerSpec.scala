/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class MinMaxNormalizerSpec extends AnyFunSuite {

  test("case 1: 2 normalizations, minValue = 0, maxValue = 1") {
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val minMaxNormalizer = new MinMaxNormalizer
    val normalizedTest = minMaxNormalizer.normalize(xs)
    val normalizedTruth = List(0.0, 0.5, 1.0)
    assert(normalizedTest == normalizedTruth)
    val newXs: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = minMaxNormalizer.normalize(newXs)
    val normalizedTruth2 = List(1.5, 2.0)
    assert(normalizedTest2 == normalizedTruth2)
  }

}