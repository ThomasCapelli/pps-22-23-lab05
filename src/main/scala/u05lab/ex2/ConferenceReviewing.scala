package u05lab.ex2

import u05lab.ex2.ConferenceReviewing.Question

trait ConferenceReviewing:

  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles: Set[Int]
  def sortedAcceptedArticles: List[(Int, Double)]
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  enum Question:
    case RELEVANCE
    case SIGNIFICANCE
    case CONFIDENCE
    case FINAL

  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  case class ConferenceReviewingImpl() extends ConferenceReviewing:

    private var reviews: List[(Int, Map[Question, Int])] = List()

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      reviews = reviews :+ (article, scores)

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      reviews = reviews :+ (article, Map(
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.RELEVANCE -> relevance,
        Question.FINAL -> fin
      ))

    override def orderedScores(article: Int, question: Question): List[Int] =
      getScoresByQuestion(article, question).sorted

    override def averageFinalScore(article: Int): Double =
      getScoresByQuestion(article, Question.FINAL).sum / reviews.count(_._1 == article).toDouble

    override def acceptedArticles: Set[Int] =
      reviews.map(_._1).filter(a => averageFinalScore(a) > 5 && getScoresByQuestion(a, Question.RELEVANCE).exists(_ >= 8)).toSet

    override def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.map(article => (article, averageFinalScore(article))).toList.sortBy(_._2)

    override def averageWeightedFinalScoreMap: Map[Int, Double] =
      reviews.groupMapReduce(_._1)(review => {
        val maximumScore = 10.0
        review._2(Question.CONFIDENCE) * review._2(Question.FINAL) / (maximumScore * reviews.count(_._1 == review._1))
      }
      )(_ + _)
      
    private def getScoresByQuestion(article: Int, question: Question): List[Int] =
      reviews.filter((a, _) => a == article).map((_, m) => m(question))

