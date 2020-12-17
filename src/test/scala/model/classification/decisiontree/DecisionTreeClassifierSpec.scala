/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import model.classification.decisiontree.DecisionTreeClassifier.entropy
import org.scalatest.funsuite.AnyFunSuite

//def entropy(yHat: Double): Double = {
//  -1.0 * yHat * math.log(yHat) - (1 - yHat) * math.log(1 - yHat)
//}

class DecisionTreeClassifierSpec extends AnyFunSuite {

  val Delta = 10e-6

  test("entropy(): case 1 - maximal entropy") {
    val entropyTest = entropy(0.5)
    assert(math.abs(entropyTest - 0.6931471805599453) < Delta)
  }

}
