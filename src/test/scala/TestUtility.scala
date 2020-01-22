object TestUtility {

  def generateBinaryExamples(numExamples: Int, numOutliers: Int): List[Example] = {
    (1 to numExamples).toList.flatMap { i => 
      var posEx = new Example(List(1, 1, 1), 1)
      var negEx = new Example(List(-1, -1, -1), -1)
      var exs = List(posEx, negEx)
      if (numOutliers > 0) {
        // throw in the outliers
        val posOutlier = new Example(List(1, 1, 1), -1)
        val negOutlier = new Example(List(-1, -1, -1), 1)
        val outliers = List(posOutlier, negOutlier)
        if (i % (numExamples / numOutliers) == 0) exs = exs ::: outliers
      }
      exs
    }
  }
  
}