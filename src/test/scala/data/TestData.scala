/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data

import example.{Example, BinaryClassificationExample, UnitBinaryClassificationExample}

object TestData {

  def generateBinaryClassificationExamples(numExamples: Int, numOutliers: Int): List[BinaryClassificationExample] = {
    (1 to numExamples).toList.flatMap { i =>
      val posEx = new BinaryClassificationExample(List(1, 1, 1), 1)
      val negEx = new BinaryClassificationExample(List(-1, -1, -1), 0)
      var exs = List(posEx, negEx)
      if (numOutliers > 0) {
        // throw in the outliers
        val posOutlier = new BinaryClassificationExample(List(1, 1, 1), 0)
        val negOutlier = new BinaryClassificationExample(List(-1, -1, -1), 1)
        val outliers = List(posOutlier, negOutlier)
        if (i % (numExamples / numOutliers) == 0) exs = exs ::: outliers
      }
      exs
    }
  }

  def generateUnitBinaryClassificationExamples(numExamples: Int,
                                               numOutliers: Int): List[UnitBinaryClassificationExample] = {
    generateBinaryClassificationExamples(numExamples, numOutliers)
      .map(e => UnitBinaryClassificationExample(new Example(e.X, if (e.y == 0) -1 else 1)))
  }
  
}