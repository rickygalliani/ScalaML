/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package example

case class PerceptronBinaryClassificationExample(override val X: List[Double], override val y: Int)
  extends BinaryClassificationExample(X, y, positiveValue = 1, negativeValue = -1)

object PerceptronBinaryClassificationExample {
  def apply(b: BinaryClassificationExample): PerceptronBinaryClassificationExample = {
    val y = {
      if (b.y == 0) { -1 }
      else if (b.y == 1) { 1 }
      else { throw new Exception(s"Unrecognized label for BinaryClassificationExample: ${b.y}") }
    }
    new PerceptronBinaryClassificationExample(b.X, y)
  }
}