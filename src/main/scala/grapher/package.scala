import cats.effect.concurrent.Ref

package object grapher {
  type State[F[_], V] = Runner[F, V]
  type Result[F[_], V] = Ref[F, Option[V]]
}