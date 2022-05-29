package grapher

trait Task[T]

trait Task0[F[_], V] extends Task[V] {
  def run(): F[V]
}

trait Task1[F[_], T, V] extends Task[V] {
  def run(t: T): F[V]
}

trait Task2[F[_], T1, T2, V] extends Task[V] {
  def run(t1: T1, t2: T2): F[V]
}
