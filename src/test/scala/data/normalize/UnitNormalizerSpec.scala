/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class UnitNormalizerSpec extends AnyFunSuite {

  test("case 1: 1 normalization") {
    val unitNormalizer = new UnitNormalizer
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val normalizedTest = unitNormalizer.normalize(0, xs)
    val normalizedTruth = List(0.0, 0.4472135954999579, 0.8944271909999159)
    assert(normalizedTest == normalizedTruth)
  }

  test("case 2: 2 normalizations") {
    val unitNormalizer = new UnitNormalizer

    val xs: List[Double] = List(0.0, 5.0, 10.0)
    unitNormalizer.normalize(0, xs)

    val newXs: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = unitNormalizer.normalize(0, newXs)
    val normalizedTruth2 = List(1.3416407864998738, 1.7888543819998317)
    assert(normalizedTest2 == normalizedTruth2)
  }

  test("case 3: 1 normalization, 2 vectors") {
    val unitNormalizer = new UnitNormalizer

    val xs1: List[Double] = List(0.0, 5.0, 10.0)
    unitNormalizer.normalize(0, xs1)

    val xs2: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = unitNormalizer.normalize(1, xs2)
    val normalizedTruth2 = List(0.6, 0.8)
    assert(normalizedTest2 == normalizedTruth2)
  }

}