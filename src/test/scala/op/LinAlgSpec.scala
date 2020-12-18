/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package op

import org.scalatest.funsuite.AnyFunSuite
import op.LinAlg.{dot, scalarMultiply}

class LinAlgSpec extends AnyFunSuite {

  test("LinAlg.scalarMultiply() - case 1") {
    val scalar = 5.0
    val vec = List(1.0, 2.0, 3.0)
    val scalarMultiplyTest = scalarMultiply(scalar, vec)
    assert(scalarMultiplyTest == List(5.0, 10.0, 15.0))
  }

  test("LinAlg.dot() - case 1") {
    val vec1 = List(1.0, 2.0, 3.0)
    val vec2 = List(2.0, 3.0, 4.0)
    val dotTest = dot(vec1, vec2)
    assert(dotTest == 20.0)
  }

}