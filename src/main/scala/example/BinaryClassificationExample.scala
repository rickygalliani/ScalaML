/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package example

class BinaryClassificationExample(override val X: List[Double],
                                  override val y: Int,
                                  positiveValue: Int = 1,
                                  negativeValue: Int = 0) extends Example(X, y) {
  assert(y == positiveValue || y == negativeValue)
}

object BinaryClassificationExample {

  def apply(e: Example, positiveValue: Int = 1, negativeValue: Int = 0): BinaryClassificationExample = {
    new BinaryClassificationExample(e.X, e.y, positiveValue, negativeValue)
  }

}



