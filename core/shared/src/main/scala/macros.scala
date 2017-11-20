package optometry

import language.higherKinds

import scala.reflect._
import scala.reflect.macros._

object LensMacros {
  def construct[A: c.WeakTypeTag](c: whitebox.Context)(path: c.Tree): c.Tree = {
    import c.universe._

    def deconstruct(path: c.Tree): List[(String, c.Tree)] = path match {
      case q"$prefix($c)" =>
        c match {
          case Literal(Constant(str: String)) =>
            (str, q"_root_.optometry.Optic.identity") :: deconstruct(prefix)
        }
      case q"$prefix.$method" if method.decodedName.toString == "selectDynamic" =>
        deconstruct(prefix)
      case q"$prefix.$method[$_, $_](${Literal(Constant(name: String))})($optic)" if method.decodedName.toString == "applyDynamic" =>
        (name, optic) :: deconstruct(prefix)
      case other =>
        Nil
    }

    def lensTree(startType: Type, returnType: Type, method: String) = {
      val getter = q"(_.${TermName(method)})"
      val setter = q"{ (a, b) => a.copy(${TermName(method)} = b(a.${TermName(method)})) }"
      q"new _root_.optometry.Lens[$startType, $returnType, $returnType]($getter, $setter)"
    }

    def dereference(method: String, typ: Type): MethodSymbol = {
      val methods = typ.typeSymbol.asType.toType.members.filter(_.isMethod).map(_.asMethod)
      methods.find(_.name.decodedName.toString == method).getOrElse {
        c.abort(c.enclosingPosition, s"optometry: $method is not a member of type $typ")
      }
    }

    def unify(optic: c.Tree, typ: Type): Type =
      c.typecheck(q"$optic.unify(null.asInstanceOf[$typ])") match {
        case q"$prefix[$t]($x)" => c.untypecheck(t).tpe
      }

    def join(dec: List[(String, c.Tree)], typ: Type): c.Tree = dec match {
      case (method, optic) :: Nil =>
        val res = dereference(method, typ)
        val lens = lensTree(typ, res.returnType, method)
        q"$optic($lens)"
        
      case (method, optic) :: tail =>
        val res = dereference(method, typ)
        val lens = lensTree(typ, res.returnType, method)
        val nextType = unify(optic, res.returnType)
        val joined = join(tail, nextType)
        q"$optic.compose($lens, $joined)"
      
      case Nil =>
        ???
    }
    
    val deconstruction = path match { case q"{ $x => $p }" => deconstruct(p) }

    join(deconstruction.reverse, weakTypeOf[A])
  }
}
