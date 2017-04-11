/**
  * Created by jbasrai on 4/10/17.
  */
object HelloWorld {
  def main(args: Array[String]): Unit = {
    val sim = new Simulation(10)
    val results = sim.start
    println(results.map(_.winner))
    println(results.head.log)
  }
}
