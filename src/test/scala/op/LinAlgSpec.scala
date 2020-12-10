/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package op

import org.scalatest.funsuite.AnyFunSuite
import op.LinAlg.dot

class LinAlgSpec extends AnyFunSuite {

  test("LinAlg.dot() - case 1") {
    val vec1 = List(1.0, 2.0, 3.0)
    val vec2 = List(2.0, 3.0, 4.0)
    val dotTest = dot(vec1, vec2)
    assert(dotTest == (2.0 + 6.0 + 12.0))
  }

}