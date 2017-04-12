/**
  * Created by jbasrai on 4/10/17.
  */
object HelloWorld {
  def main(args: Array[String]): Unit = {
    val trials = 1000
    val sim = new Simulation(new SimpleFascistStrategy, trials)
    val results = sim.start

    println(results.head.log)

    val liberalWins = results.map(_.winner).count(_ == Liberal)

    val fascistFrequencyMap = results.map(_.board.fascists).groupBy(x => x).mapValues(_.length).toList.sortBy(_._1)

    println(s"Liberal winrate: $liberalWins / $trials")
    for ((fascistPolicesEnacted, frequency) <- fascistFrequencyMap) yield {
      println(s"$fascistPolicesEnacted fascist policies enacted $frequency / $trials of the time")
    }
  }
}
