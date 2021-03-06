import io.prediction.controller.EngineFactory
import io.prediction.controller.Engine

case class Query(
  user: String,
  num: Int,
  blackList: Set[String]
)

case class PredictedResult(propertyRatings: Map[String, Double])

object RecommendationEngine extends EngineFactory {
  def apply() = {
    new Engine(
      classOf[DataSource],
      classOf[Preparator],
      Map("als" -> classOf[Algorithm]),
      classOf[Serving]
    )
  }
}
