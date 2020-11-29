/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class MinMaxNormalizerSpec extends AnyFunSuite {

  test("case 1: minValue = 0, maxValue = 1") {
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val minMaxNormalizer = new MinMaxNormalizer()
    val normalizedTest = minMaxNormalizer.normalize(xs)
    val normalizedTruth = List(0.0, 0.5, 1.0)
    assert(normalizedTest == normalizedTruth)
  }

}