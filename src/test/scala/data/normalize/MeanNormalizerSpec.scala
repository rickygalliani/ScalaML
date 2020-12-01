/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class MeanNormalizerSpec extends AnyFunSuite {

  test("case 1: 2 normalizations") {
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val meanNormalizer = new MeanNormalizer
    val normalizedTest = meanNormalizer.normalize(xs)
    val normalizedTruth = List(-0.5, 0.0, 0.5)
    assert(normalizedTest == normalizedTruth)
    val newXs: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = meanNormalizer.normalize(newXs)
    val normalizedTruth2 = List(1.0, 1.5)
    assert(normalizedTest2 == normalizedTruth2)
  }

}