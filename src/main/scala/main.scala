import cats.effect.{ExitCode, IO, IOApp, Sync}
import grapher._
import grapher.implicits._

object main extends IOApp {

  class SimpleTasks[F[_] : Sync]() {
    def task1: Task0[F, Int] = () => Sync[F].pure(12)
    def task2: Task1[F, Int, String] = (t: Int) => Sync[F].pure(t.toString + ": xx :")
    def task3: Task2[F, Int, String, String] = (t1: Int, t2: String) => Sync[F].pure(t2 + t1.toString)
  }

  def run(args: List[String]): IO[ExitCode] = {
    val tasks = new SimpleTasks[IO]

    val basicJob = Job.build[IO](
      for {
        t1 <- tasks.task1.dependsOn()
        t2 <- tasks.task2.dependsOn(t1)
        _ <- tasks.task3.dependsOn(t1, t2)
      } yield ()
    )

    for {
      job <- basicJob
      _ <- job.run
      _ <- job.state
    } yield ExitCode.Success
  }
}