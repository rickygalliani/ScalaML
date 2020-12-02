/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class MeanNormalizerSpec extends AnyFunSuite {

  test("case 1: 1 normalization") {
    val meanNormalizer = new MeanNormalizer
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val normalizedTest = meanNormalizer.normalize(0, xs)
    val normalizedTruth = List(-0.5, 0.0, 0.5)
    assert(normalizedTest == normalizedTruth)
  }

  test("case 2: 2 normalizations") {
    val meanNormalizer = new MeanNormalizer

    val xs: List[Double] = List(0.0, 5.0, 10.0)
    meanNormalizer.normalize(0, xs)

    val newXs: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = meanNormalizer.normalize(0, newXs)
    val normalizedTruth2 = List(1.0, 1.5)
    assert(normalizedTest2 == normalizedTruth2)
  }

  test("case 3: 1 normalization, 2 vectors") {
    val meanNormalizer = new MeanNormalizer

    val xs1: List[Double] = List(0.0, 5.0, 10.0)
    meanNormalizer.normalize(0, xs1)

    val xs2: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = meanNormalizer.normalize(1, xs2)
    val normalizedTruth2 = List(-0.5, 0.5)
    assert(normalizedTest2 == normalizedTruth2)
  }

}