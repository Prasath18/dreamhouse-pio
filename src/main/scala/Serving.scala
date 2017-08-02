import io.prediction.controller.LServing
import com.github.fommil.netlib.BLAS.{getInstance => blas}

class Serving extends LServing[Query, PredictedResult] {

  override def serve(query: Query, predictedResults: Seq[PredictedResult]): PredictedResult = {
    predictedResults.head
    
     def recommendProductsWithFilter(user: Int, num: Int, productIdFilter: Set[Int]) = {
    val filteredProductFeatures = productFeatures
      .filter { case (id, _) => !productIdFilter.contains(id) } // (*)
    recommend(userFeatures.lookup(user).head, filteredProductFeatures, num)
      .map(t => Rating(user, t._1, t._2))
  }
    
    private def recommend(
      recommendToFeatures: Array[Double],
      recommendableFeatures: RDD[(Int, Array[Double])],
      num: Int): Array[(Int, Double)] = {
    val scored = recommendableFeatures.map { case (id, features) =>
      (id, blas.ddot(features.length, recommendToFeatures, 1, features, 1))
    }
    scored.top(num)(Ordering.by(_._2))
  }
    
  }
}
