/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

import org.scalatest.funsuite.AnyFunSuite

class ZScoreNormalizerSpec extends AnyFunSuite {

  test("case 1") {
    val xs: List[Double] = List(0.0, 5.0, 10.0)
    val zScoreNormalizer = new ZScoreNormalizer(xs)
    val normalizedTest = zScoreNormalizer.normalize(xs)
    val normalizedTruth = List(-1.224744871391589, 0.0, 1.224744871391589)
    assert(normalizedTest == normalizedTruth)
  }

}