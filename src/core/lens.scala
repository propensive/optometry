package optometry

import language.higherKinds
import language.dynamics
import language.experimental.macros

final class Lens[A, B, B2](get: A => B2, set: (A, B => B) => A) {
  def apply(a: A): B2 = get(a)
  def update(a: A, b: B): A = set(a, _ => b)
  def modify(a: A)(b: B => B): A = set(a, b)
}

abstract class Optic[F[_], G[_], A](name: String) {

  /** defines how the optic should map across the values */
  def map[B](v: F[A])(fn: A => B): G[B]
  
  def comap(f: F[A], g: G[A]): F[A] 

  /** maps this optic to the lens */
  def apply[B](lens: Lens[B, F[A], F[A]]): Lens[B, A, G[A]] = new Lens[B, A, G[A]](
    { a => map(lens(a))(identity) },
    { (a, b) =>
      val la = lens(a)
      lens(a) = comap(la, map(la)(b))
    }
  )

  /** combines two lenses using this optic */
  def compose[B, C, C2](left: Lens[B, F[A], F[A]], right: Lens[A, C, C2]): Lens[B, C, G[C2]] = {
    new Lens[B, C, G[C2]](
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

object Optic {

  def identity[A]: Optic[optics.Id, optics.Id, A] = new Optic[({ type L[T] = T })#L, ({ type L[T] = T })#L, A]("") {
    def map[B](v: A)(fn: A => B): B = fn(v)
    def comap(f: A, g: A): A = g
  }
}

object optics {
  
  type Id[T] = T
  
  def each[A]: Optic[List, List, A] = new Optic[List, List, A]("each") {
    def map[B](v: List[A])(fn: A => B): List[B] = v.map(fn)
    def comap(f: List[A], g: List[A]): List[A] = g
  }
  
  def option[A]: Optic[Option, Option, A]= new Optic[Option, Option, A]("option") {
    def map[B](v: Option[A])(fn: A => B): Option[B] = v.map(fn)
    def comap(f: Option[A], g: Option[A]): Option[A] = g
  }
  
  def headOption[A]: Optic[List, Option, A] = new Optic[List, Option, A]("option") {
    def map[B](v: List[A])(fn: A => B): Option[B] = v.headOption.map(fn)
    def comap(f: List[A], g: Option[A]): List[A] = f match {
      case Nil => Nil
      case h :: t => g.to[List] ::: t
    }
  }
  
  def one[A](pred: A => Boolean): Optic[List, Id, A] =
    new Optic[List, Id, A]("focus") {
      def map[B](v: List[A])(fn: A => B): B = fn(v.find(pred).get)
      def comap(f: List[A], g: A): List[A] = {
        val idx = f.indexWhere(pred)
        f.take(idx) ::: (g :: f.drop(idx + 1))
      }
    }
  
  def focus[A](pred: A => Boolean): Optic[List, Option, A] =
    new Optic[List, Option, A]("focus") {
      def map[B](v: List[A])(fn: A => B): Option[B] = v.find(pred).map(fn)
      def comap(f: List[A], g: Option[A]): List[A] = {
        val idx = f.indexWhere(pred)
        f.take(idx) ::: g.to[List] ::: f.drop(idx + 1)
      }
    }
}

object Lens {

  def apply[A]: Partial[A] = new Partial[A]()

  class Partial[A]() {
    def apply[B](path: Dyn => Dyn): Any = macro LensMacros.construct[A]
    def lens[B](path: Dyn => Dyn): Any = macro LensMacros.construct[A]
  }

  private class Dyn extends Dynamic {
    def selectDynamic(name: String): Dyn = ???
    def applyDynamic[Fn[_], Fn2[_], A](name: String)(app: Optic[Fn, Fn2, A]): Dyn = ???
  }
}
