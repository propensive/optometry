package optometry

import language.higherKinds
import language.dynamics
import language.experimental.macros

class Lens[A, B, B2](get: A => B2, set: (A, B => B) => A) {
  def apply(a: A): B2 = get(a)
  def update(a: A, b: B): A = set(a, _ => b)
  def modify(a: A)(b: B => B): A = set(a, b)
}

abstract class Optic[F[_]](name: String) {
  /** maps this optic to the lens */
  def apply[A, B](lens: Lens[A, F[B], F[B]]): Lens[A, B, F[B]] = new Lens[A, B, F[B]](
    lens.apply,
    (a, b) => lens(a) = map(lens(a))(b)
  )

  /** defines how the optic should map across the values */
  def map[A, B](v: F[A])(fn: A => B): F[B]
 
  /** combines two lenses using this optic */
  def compose[A, B, C, C2](left: Lens[A, F[B], F[B]], right: Lens[B, C, C2]): Lens[A, C, F[C2]] = {
    new Lens[A, C, F[C2]](
      { a => map(left(a))(right.apply) },
      { (a, c) => left.update(a, map(left(a))(x => right.modify(x)(c))) }
    )
  }

  /** this method exists only for the purposes of assisting the macro to typecheck */
  def unify[T](x: F[T]): Nothing = ???
}

object Optic {
  object identity extends Optic[({ type L[T] = T})#L]("") {
    def map[A, B](v: A)(fn: A => B): B = fn(v)
  }
}

object each extends Optic[List]("each") {
  def map[A, B](v: List[A])(fn: A => B): List[B] = v.map(fn)
}

object option extends Optic[Option]("option") {
  def map[A, B](v: Option[A])(fn: A => B): Option[B] = v.map(fn)
}

object Lens {

  type Id[T] = T

  def apply[A]: PartialLens[A] = new PartialLens[A]()

  class PartialLens[A]() {
    def apply[B](path: Dyn => Dyn): Any = macro LensMacros.construct[A]
  }

  private class Dyn extends Dynamic {
    def selectDynamic(name: String): Dyn = ???
    def applyDynamic[Fn[_]](name: String)(app: Optic[Fn]): Dyn = ???
  }
}
