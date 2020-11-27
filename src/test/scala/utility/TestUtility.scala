/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package utility

import example.Example

object TestUtility {

  def generateBinaryExamples(numExamples: Int, numOutliers: Int): List[Example] = {
    (1 to numExamples).toList.flatMap { i =>
      val posEx = Example(List(1, 1, 1), 1)
      val negEx = Example(List(-1, -1, -1), -1)
      var exs = List(posEx, negEx)
      if (numOutliers > 0) {
        // throw in the outliers
        val posOutlier = Example(List(1, 1, 1), -1)
        val negOutlier = Example(List(-1, -1, -1), 1)
        val outliers = List(posOutlier, negOutlier)
        if (i % (numExamples / numOutliers) == 0) exs = exs ::: outliers
      }
      exs
    }
  }
  
}