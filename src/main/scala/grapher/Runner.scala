package grapher

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._

trait Runner[F[_], V] {
  def result: Result[F, V]
  def withResult(r: Option[V]): F[Unit] = result.update(_ => r)
  def isReady: F[Boolean]
  def run: F[Unit]
}

case class Runner0[F[_]: Sync, V](task: Task0[F, V], result: Result[F, V]) extends Runner[F, V] {
  def isReady: F[Boolean] = Sync[F].pure(true)
  override def run: F[Unit] = for {
    r <- task.run()
    _ <- withResult(Some(r))
  } yield ()
}

case class Runner1[F[_]: Sync, T, V](task: Task1[F, T, V], dep: Result[F, T], result: Result[F, V]) extends Runner[F, V] {
  def isReady: F[Boolean] = for {
    d <- dep.get
  } yield d.isDefined

  override def run: F[Unit] = for {
    d <- dep.get
    r = for {
      p <- d
    } yield task.run(p)
    v <- r match {
      case None => Sync[F].pure(None)
      case Some(v) => v.map(Option(_))
    }
    _ <- withResult(v)
  } yield ()
}

case class Runner2[F[_]: Sync, T1, T2, V](task: Task2[F, T1, T2, V], dep1: Result[F, T1], dep2: Result[F, T2], result: Result[F, V]) extends Runner[F, V] {
  def isReady: F[Boolean] = for {
    d1 <- dep1.get
    d2 <- dep2.get
  } yield d1.isDefined && d2.isDefined

  override def run: F[Unit] = for {
    d1 <- dep1.get
    d2 <- dep2.get
    r = for {
      p1 <- d1
      p2 <- d2
    } yield task.run(p1, p2)
    v <- r match {
      case None => Sync[F].pure(None)
      case Some(v) => v.map(Option(_))
    }
    _ <- withResult(v)
  } yield ()
}
