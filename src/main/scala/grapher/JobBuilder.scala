package grapher

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.flatMap._
import cats.syntax.functor._

trait TDep[F[_]]
case class Non[F[_], V](f: State[F, V]) extends TDep[F]
case class Dep[F[_], V, T](f: State[F, V], t: F[TDep[F]]) extends TDep[F]

case class Cont[F[_], V](state: F[Runner[F, V]])

object JobBuilder {

  def extract[F[_] : Sync](d: F[TDep[F]]): F[List[State[F, _]]] = {
    for {
      ds <- d
      z <- ds match {
        case Non(f) => Sync[F].pure(List(f))
        case Dep(f, t) => for {
          dd <- extract(t)
        } yield f +: dd
      }
    } yield z
  }
}

object implicits {

  implicit class Task0Ops[F[_] : Sync, V](task: Task0[F, V]) {
    def dependsOn(): Cont[F, V] = {
      val v: F[Runner[F, V]] = for {
        r <- Ref[F].of(Option.empty[V])
      } yield Runner0(task, r)

      Cont(v)
    }
  }

  implicit class Task1Ops[F[_] : Sync, T, V](task: Task1[F, T, V]) {
    def dependsOn(t: Result[F, T]): Cont[F, V] = {
      val v: F[Runner[F, V]] = for {
        r <- Ref[F].of(Option.empty[V])
      } yield Runner1(task, t, r)

      Cont(v)
    }
  }

  implicit class Task2Ops[F[_] : Sync, T1, T2, V](task: Task2[F, T1, T2, V]) {
    def dependsOn(t1: Result[F, T1], t2: Result[F, T2]): Cont[F, V] = {
      val v: F[Runner[F, V]] = for {
        r <- Ref[F].of(Option.empty[V])
      } yield Runner2(task, t1, t2, r)

      Cont(v)
    }
  }

  implicit class RunnerOps[F[_] : Sync, V](cont: Cont[F, V]) {
    def flatMap(f: Result[F, V] => F[TDep[F]]): F[TDep[F]] = for {
      s1 <- cont.state
      s2 = f(s1.result)
    } yield Dep(s1, s2)

    def map(f: Result[F, V] => Unit): F[TDep[F]] = for {
      s1 <- cont.state
    } yield Non(s1)

  }
}