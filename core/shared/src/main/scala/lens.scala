package optometry

import language.higherKinds
import language.dynamics
import language.experimental.macros

final class Lens[A, B, B2](get: A => B2, set: (A, B => B) => A) {
  def apply(a: A): B2 = get(a)
  def update(a: A, b: B): A = set(a, _ => b)
  def modify(a: A)(b: B => B): A = set(a, b)
}

abstract class Profunctor[F[_], G[_]](name: String) {

  /** defines how the profunctor should map across the values */
  def map[A, B](v: F[A])(fn: A => B): G[B]
  
  def comap[A](f: F[A], g: G[A]): F[A] 

  /** maps this profunctor to the lens */
  def apply[A, B](lens: Lens[A, F[B], F[B]]): Lens[A, B, G[B]] = new Lens[A, B, G[B]](
    { a => map(lens(a))(identity) },
    { (a, b) =>
      val la = lens(a)
      lens(a) = comap(la, map(la)(b))
    }
  )
 
  /** combines two lenses using this profunctor */
  def compose[A, B, C, C2](left: Lens[A, F[B], F[B]], right: Lens[B, C, C2]): Lens[A, C, G[C2]] = {
    new Lens[A, C, G[C2]](
      { a => map(left(a))(right.apply) },
      { (a, c) =>
        val la = left(a)
        left(a) = comap(la, map(la)(right.modify(_)(c)))
      }
    )
  }

  /** this method exists only for the purposes of assisting the macro to typecheck */
  def unify[T](x: F[T]): Nothing = ???
}

object Profunctor {

  object identity extends Profunctor[({ type L[T] = T })#L, ({ type L[T] = T })#L]("") {
    def map[A, B](v: A)(fn: A => B): B = fn(v)
    def comap[A](f: A, g: A): A = g
  }
}

object each extends Profunctor[List, List]("each") {
  def map[A, B](v: List[A])(fn: A => B): List[B] = v.map(fn)
  def comap[A](f: List[A], g: List[A]): List[A] = g
}

object option extends Profunctor[Option, Option]("option") {
  def map[A, B](v: Option[A])(fn: A => B): Option[B] = v.map(fn)
  def comap[A](f: Option[A], g: Option[A]): Option[A] = g
}

object headOption extends Profunctor[List, Option]("option") {
  def map[A, B](v: List[A])(fn: A => B): Option[B] = v.headOption.map(fn)
  def comap[A](f: List[A], g: Option[A]): List[A] = f match {
    case Nil => Nil
    case h :: t => g.to[List] ::: t
  }
}

object Lens {

  def apply[A]: PartialLens[A] = new PartialLens[A](0)

  final class PartialLens[A](private val nothing: Int) extends AnyVal {
    def apply[B](path: Dyn => Dyn): Any = macro LensMacros.construct[A]
  }

  private class Dyn extends Dynamic {
    def selectDynamic(name: String): Dyn = ???
    def applyDynamic[Fn[_], Fn2[_]](name: String)(app: Profunctor[Fn, Fn2]): Dyn = ???
  }
}
