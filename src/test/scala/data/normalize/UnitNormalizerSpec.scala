/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class UnitNormalizerSpec extends AnyFunSuite {

  test("case 1: 2 normalizations") {
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val unitNormalizer = new UnitNormalizer
    val normalizedTest = unitNormalizer.normalize(xs)
    val normalizedTruth = List(0.0, 0.4472135954999579, 0.8944271909999159)
    assert(normalizedTest == normalizedTruth)
    val newXs: List[Double] = List(15.0, 20.0)
    val normalizedTest2 = unitNormalizer.normalize(newXs)
    val normalizedTruth2 = List(1.3416407864998738, 1.7888543819998317)
    assert(normalizedTest2 == normalizedTruth2)
  }

}