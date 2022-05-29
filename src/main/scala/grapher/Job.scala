package grapher

import cats.effect.Sync
import cats.syntax.functor._
import cats.syntax.traverse._

case class Job[F[_] : Sync](tasks: List[State[F, _]]) {
  def run: F[Unit] =
    tasks.traverse(_.run).void

  def state: F[Unit] =
    tasks.traverse(_.result.get.map(v => println(s" [RES] $v"))).void
}

object Job {
  def build[F[_]: Sync](builder: F[TDep[F]]): F[Job[F]] =
    JobBuilder.extract(builder).map(Job(_))
}
