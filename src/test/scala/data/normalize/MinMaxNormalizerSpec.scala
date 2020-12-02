/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class MinMaxNormalizerSpec extends AnyFunSuite {

  test("case 1: 1 normalization, minValue = 0, maxValue = 1") {
    val minMaxNormalizer = new MinMaxNormalizer
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val normalizedTest = minMaxNormalizer.normalize(0, xs)
    val normalizedTruth = List(0.0, 0.5, 1.0)
    assert(normalizedTest == normalizedTruth)
  }

  test("case 2: 2 normalizations, minValue = 0, maxValue = 1") {
    val minMaxNormalizer = new MinMaxNormalizer

    val xs: List[Double] = List(0.0, 5.0, 10.0)
    minMaxNormalizer.normalize(0, xs)

    val newXs: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = minMaxNormalizer.normalize(0, newXs)
    val normalizedTruth2 = List(1.5, 2.0)
    assert(normalizedTest2 == normalizedTruth2)
  }

  test("case 3: 1 normalization, 2 vectors, minValue = 0, maxValue = 1") {
    val minMaxNormalizer = new MinMaxNormalizer

    val xs1: List[Double] = List(0.0, 5.0, 10.0)
    minMaxNormalizer.normalize(0, xs1)

    val xs2: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = minMaxNormalizer.normalize(1, xs2)
    val normalizedTruth2 = List(0.0, 1.0)
    assert(normalizedTest2 == normalizedTruth2)
  }

}