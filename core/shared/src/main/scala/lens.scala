package optometry

import language.higherKinds
import language.dynamics

import language.experimental.macros

import scala.reflect._
import scala.reflect.macros._

class Lens[A, B, B2](get: A => B2, set: (A, B => B) => A) {
  def apply(a: A): B2 = get(a)
  def update(a: A, b: B): A = set(a, _ => b)
  def modify(a: A)(b: B => B): A = set(a, b)
}

trait Profunctor[F[_]] {
  /** maps this profunctor to the lens */
  def apply[A, B](lens: Lens[A, F[B], F[B]]): Lens[A, B, F[B]] =
    new Lens[A, B, F[B]](
      lens.apply,
      (a, b) => lens(a) = map(lens(a))(b)
    )

  /** defines how the profunctor should map across the values */
  def map[A, B](v: F[A])(fn: A => B): F[B]
 
  /** combines two lenses using this profunctor */
  def compose[A, B, C, C2](left: Lens[A, F[B], F[B]], right: Lens[B, C, C2]): Lens[A, C, F[C2]] = {
    new Lens[A, C, F[C2]](a => map(left(a))(right.apply), (a, c) => left.update(a, map(left(a))(x => right.modify(x)(c))))
  }

  /** this method exists only for the purposes of assisting the macro to typecheck */
  def unify[T](x: F[T]): Nothing = ???
}

object IdProfunctor extends Profunctor[({ type L[T] = T})#L] {
  def map[A, B](v: A)(fn: A => B): B = fn(v)
}

object each extends Profunctor[List] {
  def map[A, B](v: List[A])(fn: A => B): List[B] = v.map(fn)
}

object option extends Profunctor[Option] {
  def map[A, B](v: Option[A])(fn: A => B): Option[B] = v.map(fn)
}

object Lens {

  type Id[T] = T

  def apply[A]: PartialLens[A] = new PartialLens[A]()

  class PartialLens[A]() {
    def apply[B](path: Dyn => Dyn): Any = macro LensMacros.construct[A]
  }
}

class Dyn extends Dynamic {
  def selectDynamic(name: String): Dyn = ???
  def applyDynamic[Fn[_]](name: String)(app: Profunctor[Fn]): Dyn = ???
}

object LensMacros {
  def construct[A: c.WeakTypeTag](c: whitebox.Context)(path: c.Tree): c.Tree = {
    import c.universe._

    def deconstruct(path: c.Tree): List[(String, c.Tree, c.Tree)] = path match {
      case q"$prefix($c)" =>
        c match {
          case Literal(Constant(str: String)) =>
            (str, tq"({ type L[T] = T })#L", q"_root_.optometry.IdProfunctor") :: deconstruct(prefix)
        }
      case q"$prefix.$method" if method.decodedName.toString == "selectDynamic" =>
        deconstruct(prefix)
      case q"$prefix.$method[$x](${Literal(Constant(b: String))})($optic)" if method.decodedName.toString == "applyDynamic" =>
        (b, x, optic) :: deconstruct(prefix)
      case other =>
        Nil
    }

    def lensTree(startType: Type, returnType: Type, method: String) = {
      val getter = q"(_.${TermName(method)})"
      val setter = q"{ (a, b) => a.copy(${TermName(method)} = b(a.${TermName(method)})) }"

      q"new _root_.optometry.Lens[$startType, $returnType, $returnType]($getter, $setter)"
    }

    def resolve(method: String, typ: Type): MethodSymbol =
      typ.typeSymbol.asType.toType.members.filter(_.isMethod).map(_.asMethod).find(_.asMethod.name.decodedName.toString == method).getOrElse {
        c.abort(c.enclosingPosition, s"optometry: $method is not a member of type $typ")
      }

    def unify(profunctor: c.Tree, typ: Type): Type =
      c.typecheck(q"$profunctor.unify(null.asInstanceOf[$typ])") match {
        case q"$prefix[$t]($x)" => c.untypecheck(t).tpe
      }

    def join(dec: List[(String, c.Tree, c.Tree)], typ: Type): c.Tree = dec match {
      case (method, b, profunctor) :: Nil =>
        val res = resolve(method, typ)
        q"$profunctor(${lensTree(typ, res.returnType, method)})"
        
      case (method, b, profunctor) :: tail =>
        val res = resolve(method, typ)
        val lens = lensTree(typ, res.returnType, method)
        val nextType = unify(profunctor, res.returnType)
        unify(profunctor, res.returnType)

        val joined = join(tail, nextType)
        val result = q"$profunctor.compose($lens, $joined)"
        result
      
      case Nil =>
        ???
    }
    
    val deconstruction = path match { case q"{ $x => $p }" => deconstruct(p) }

    join(deconstruction.reverse, weakTypeOf[A])
  }
}
