/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model

import example.{BinaryClassificationExample, Example, UnitBinaryClassificationExample}

abstract class BinaryClassificationModel extends Model[BinaryClassificationExample]{

  def preprocess(examples: List[Example]): List[BinaryClassificationExample] = {
    examples.map(e => BinaryClassificationExample(e))
  }

  protected def learn(examples: List[BinaryClassificationExample]): Unit

  def predict(X: List[Double]): Int

}

abstract class UnitBinaryClassificationModel extends Model[UnitBinaryClassificationExample]{

  def preprocess(examples: List[Example]): List[UnitBinaryClassificationExample] = {
    examples.map(e => UnitBinaryClassificationExample(e))
  }

  protected def learn(examples: List[UnitBinaryClassificationExample]): Unit

  def predict(X: List[Double]): Int

}

