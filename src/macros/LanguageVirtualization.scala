package org.scala_lang.virtualized

import scala.reflect.macros.blackbox.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Converts Scala features that can not be overridden to method calls that can be given
 * arbitrary semantics.
 *
 * ==Features covered are==
 * {{{
 *   var x = e              =>       var x = __newVar(e)
 *   if(c) t else e         =>       __ifThenElse(c, t, e)
 *   return t               =>       __return(t)
 *   x = t                  =>       __assign(x, t)
 *   while(c) b             =>       __whileDo(c, b)
 *   do b while c           =>       __doWhile(c, b)
 * }}}
 *
 * ===Poor man's infix methods for `Any` methods===
 * {{{
 *   t == t1                =>       infix_==(t, t1)
 *   t != t1                =>       infix_!=(t, t1)
 *   t.##                   =>       infix_##(t, t1)
 *   t.equals t1            =>       infix_equals(t, t1)
 *   t.hashCode             =>       infix_hashCode(t)
 *   t.asInstanceOf[T]      =>       infix_asInstanceOf[T](t)
 *   t.isInstanceOf[T]      =>       infix_isInstanceOf[T](t)
 *   t.toString             =>       infix_toString(t)
 *   t.getClass             =>       infix_getClass(t)
 * }}}
 *
 * ===Poor man's infix methods for `AnyRef` methods===
 * {{{
 *   t eq t1                =>       infix_eq(t, t1)
 *   t ne t1                =>       infix_ne(t, t1)
 *   t.clone                =>       infix_clone(t)
 *   t.notify               =>       infix_notify(t)
 *   t.notifyAll            =>       infix_notifyAll(t)
 *   t.synchronized[T](t1)  =>       infix_synchronized(t, t1)
 *   t.wait                 =>       infix_wait(t)
 *   t.wait(l)              =>       infix_wait(t, l)
 *   t.wait(t1, l)          =>       infix_wait(t, t1, l)
 *   t.finalize()           =>       infix_finalize(t)
 * }}}
 *
 * @todo
 * {{{
 *   try b catch c          =>       __tryCatch(b, c, f)
 *   throw e                =>       __throw(e)
 *   case class C { ... }   =>       ???
 *   Nothing                =>       ???
 *   Null                   =>       ???
 * }}}
 */
trait LanguageVirtualization extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._

  def virtualize(t: Tree): (List[Tree], Seq[DSLFeature]) = VirtualizationTransformer(t)

  object VirtualizationTransformer {
    def apply(tree: Tree) = {
      val t = new VirtualizationTransformer().apply(tree)
      log("(virtualized, Seq[Features]): " + t, 2)
      t
    }
  }

  private class VirtualizationTransformer extends Transformer {
    val lifted = mutable.ArrayBuffer[DSLFeature]()

    def liftFeature(receiver: Option[Tree], nme: String, args: List[Tree], targs: List[Tree] = Nil, trans: Tree => Tree = transform): Tree = {
      lifted += DSLFeature(receiver.map(_.tpe), nme, targs, List(args.map(_.tpe)))
      log(show(method(receiver.map(trans), nme, List(args.map(trans)), targs)), 3)
      method(receiver.map(trans), nme, List(args.map(trans)), targs)
    }

    // this is used for scopes:
    // def OptiQL[R](b: => R) = new Scope[OptiQLLower, OptiQLLowerRunner[R], R](b)
    // syntax has to correspond exactly!
    // this map collects dsls at definition site so it can access them at call site
    // TODO: this is not a safe feature which should rather be removed as it is not longer used in Delite
    val dslScopes:scala.collection.mutable.HashMap[String, (Tree, Tree, Tree)] = new scala.collection.mutable.HashMap()

    // Call for transforming Blocks. Allows expanding a single statement to multiple statements within a given scope
    private def transformStm(tree: Tree): List[Tree] = tree match {
      // TODO: Name mangling is nice and elegant, but becomes an issue when we assume we've mangled
      // names which actually haven't been changed. Would need to come up with a solution to check to see
      // if a name's been mangled that also respects scoping (and potentially incremental compilation?)
      /*
      case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
        // Mangle Var name to make readVar calls happen explicitly
        val s = TermName(sym+"$v")
        // leaving it a var makes it easier to revert when custom __newVar isn't supplied
        val v = ValDef(mods, s, tpt, liftFeature(None, "__newVar", List(rhs)))
        val d = DefDef(mods, sym, Nil, Nil, tpt, liftFeature(None, "__readVar", List(Ident(s))))

        List(v, d)
      */

      case _ => List(transform(tree))
    }


    override def transform(tree: Tree): Tree = atPos(tree.pos) {
      tree match {
        /* Attempt to virtualize vars in both class bodies and blocks **/
        case Template(parents, selfType, bodyList) =>
          val body = bodyList.flatMap(transformStm)

          Template(parents, selfType, body)

        case Block(stms, ret) =>
          val stms2 = stms.flatMap(transformStm) ++ transformStm(ret)

          Block(stms2.dropRight(1), stms2.last)


        /* Variables */
        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          // TODO: What about case like:
          // var x: Option[Int] = None
          // x = Some(3)
          // __newVar: Var[Option[Int]]
          ValDef(mods, sym, tpt, liftFeature(None, "__newVar", List(rhs)))

        case Assign(lhs, rhs) =>
          liftFeature(None, "__assign", List(lhs, rhs))

        // Don't rewrite +=, -=, *=, and /=. This restricts the return value to Unit
        // in the case where the compiler/DSL author chooses to use implicit classes rather
        // than infix_ methods
        /*
        case Apply(Select(qualifier, TermName("$plus$eq")), List(arg)) =>     // x += y
          liftFeature(None, "infix_$plus$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$minus$eq")), List(arg)) =>    // x -= y
          liftFeature(None, "infix_$minus$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$times$eq")), List(arg)) =>    // x *= y
          liftFeature(None, "infix_$times$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$div$eq")), List(arg)) =>      // x /= y
          liftFeature(None, "infix_$div$eq", List(qualifier, arg))
        */


        // Name mangling version
        /*case Assign(Ident(lhs), rhs) =>
          liftFeature(None, "__assign", List(Ident(lhs+"$v"), rhs))*/


        /* Control structures (keywords) */

        case t @ If(cond, thenBr, elseBr) =>
          liftFeature(None, "__ifThenElse", List(cond, thenBr, elseBr))

        case Return(e) =>
          liftFeature(None, "__return", List(e))

        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label),
          List())), Literal(Constant(())))) if label == sym => // while(){}
          liftFeature(None, "__whileDo", List(cond, body))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label),
          List()), Literal(Constant(()))))) if label == sym => // do while(){}
          liftFeature(None, "__doWhile", List(cond, body))

        case Try(block, catches, finalizer) => {
          c.warning(tree.pos, "virtualization of try/catch expressions is not supported.")
          super.transform(tree)
        }

        case Throw(expr) => {
          c.warning(tree.pos, "virtualization of throw expressions is not supported.")
          super.transform(tree)
        }

        /* Special case + for String literals */

        // only virtualize `+` to `infix_+` if lhs is a String *literal* (we can't look at types!)
        // NOFIX: this pattern does not work for: `string + unstaged + staged`
        case Apply(Select(qual @ Literal(Constant(s: String)), TermName("$plus")), List(arg)) =>
          liftFeature(None, "infix_$plus", List(qual, arg))

        /* Methods defined on Any/AnyRef with arguments */

        case Apply(Select(qualifier, TermName("$eq$eq")), List(arg)) =>
          liftFeature(None, "infix_$eq$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$bang$eq")), List(arg)) =>
          liftFeature(None, "infix_$bang$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("equals")), List(arg)) =>
          liftFeature(None, "infix_equals", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("eq")), List(arg)) =>
          liftFeature(None, "infix_eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("ne")), List(arg)) =>
          liftFeature(None, "infix_ne", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("wait")), List(arg)) =>
          liftFeature(None, "infix_wait", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("wait")), List(arg0, arg1)) =>
          liftFeature(None, "infix_wait", List(qualifier, arg0, arg1))

        case Apply(Select(qualifier, TermName("synchronized")), List(arg)) =>
          liftFeature(None, "infix_synchronized", List(qualifier, arg))

        case Apply(TypeApply(Select(qualifier, TermName("synchronized")), targs), List(arg)) =>
          liftFeature(None, "infix_synchronized", List(qualifier, arg), targs)

        case TypeApply(Select(qualifier, TermName("asInstanceOf")), targs) =>
          liftFeature(None, "infix_asInstanceOf", List(qualifier), targs)

        case TypeApply(Select(qualifier, TermName("isInstanceOf")), targs) =>
          liftFeature(None, "infix_isInstanceOf", List(qualifier), targs)

        /* Methods defined on Any/AnyRef without arguments */

        // For 0-arg methods we get a different tree depending on if the user writes empty parens 'x.clone()' or no parens 'x.clone'
        // We always match on the empty parens version first

        case Apply(Select(qualifier, TermName("toString")), List()) =>
          liftFeature(None, "infix_toString", List(qualifier))

        case Select(qualifier, TermName("toString")) =>
          liftFeature(None, "infix_toString", List(qualifier))

        case Apply(Select(qualifier, TermName("$hash$hash")), List()) =>
          liftFeature(None, "infix_$hash$hash", List(qualifier))

        case Select(qualifier, TermName("$hash$hash")) =>
          liftFeature(None, "infix_$hash$hash", List(qualifier))

        case Apply(Select(qualifier, TermName("hashCode")), List()) =>
          liftFeature(None, "infix_hashCode", List(qualifier))

        case Select(qualifier, TermName("hashCode")) =>
          liftFeature(None, "infix_hashCode", List(qualifier))

        case Apply(Select(qualifier, TermName("clone")), List()) =>
          liftFeature(None, "infix_clone", List(qualifier))

        case Select(qualifier, TermName("clone")) =>
          liftFeature(None, "infix_clone", List(qualifier))

        case Apply(Select(qualifier, TermName("notify")), List()) =>
          liftFeature(None, "infix_notify", List(qualifier))

        case Select(qualifier, TermName("notify")) =>
          liftFeature(None, "infix_notify", List(qualifier))

        case Apply(Select(qualifier, TermName("notifyAll")), List()) =>
          liftFeature(None, "infix_notifyAll", List(qualifier))

        case Select(qualifier, TermName("notifyAll")) =>
          liftFeature(None, "infix_notifyAll", List(qualifier))

        case Apply(Select(qualifier, TermName("wait")), List()) =>
          liftFeature(None, "infix_wait", List(qualifier))

        case Select(qualifier, TermName("wait")) =>
          liftFeature(None, "infix_wait", List(qualifier))

        case Apply(Select(qualifier, TermName("finalize")), List()) =>
          liftFeature(None, "infix_finalize", List(qualifier))

        case Select(qualifier, TermName("finalize")) =>
          liftFeature(None, "infix_finalize", List(qualifier))

        case Apply(Select(qualifier, TermName("getClass")), List()) =>
          liftFeature(None, "infix_getClass", List(qualifier))

        case Select(qualifier, TermName("getClass")) =>
          liftFeature(None, "infix_getClass", List(qualifier))

        /* Unsupported */

        case ClassDef(mods, name, tpt, body) if mods.hasFlag(Flag.CASE) =>
          // sstucki: there are issues with the ordering of
          // virtualization and expansion of case classes (i.e. some
          // of the expanded code might be virtualized even though it
          // should not be and vice-versa).  So until we have decided
          // how proper virtualization of case classes should be done,
          // any attempt to do so should fail.
          // TR: not 100% sure what the issue is (although i vaguely
          // remember that we had issues in Scala-Virtualized with
          // auto-generated case class equality methods using virtualized
          // equality where it shouldn't). For the moment it seems like
          // just treating case classes as regular classes works fine.
          c.warning(tree.pos, "virtualization of case classes is not fully supported.")
          super.transform(tree) //don't virtualize the case class definition but virtualize its body

        /* Scopes */

        // this is a helper `method` for DSL generation in Forge
        // It avoid some boilerplate code but it not that principled:
        // USAGE:
        // magic() //have to make an explicit call to 'execute' side effects
        // @virtualize //values could not be annotated...
        // def magic[R]() = withTpee(Community){ //rhs pattern is matched by virtualized

        case Apply(Apply(Ident(TermName("withTpee")), List(termName)), body) =>
          val objName = TermName(termName.toString()+"Object")
          //TODO (macrotrans) val bodyTransform = transform(body)
          val x = q"""
            _tpeScopeBox = $termName
            abstract class DSLprog extends TpeScope {
              def apply = $body //Transform
            }
            class DSLrun extends DSLprog with TpeScopeRunner
            ((new DSLrun): TpeScope with TpeScopeRunner).result
          """
          c.warning(tree.pos, s"WITHTPE SCOPE GENERATED for term: "+termName.toString)
          x

        /**
        * little Hack for Delite Scope object:
        * inject specific Code for DSL to make it easier to use a DSL
        *
        * given:
        * `def OptiML[R](b: => R) = new Scope[OptiML, OptiMLExp, R](b)`
        *
        * generate:
        * `OptiML { body }` is expanded to:
        *
        * trait DSLprog$ extends OptiML {def apply = body}
        * (new DSLprog$ with OptiMLExp): OptiML with OptiMLExp
        *
        * other use case: (with type parameters)
        * new Scope[TpeScope, TpeScopeRunner[R], R](block)
        * Apply(Select(New(AppliedTypeTree(Ident(TypeName("Scope")), List(id1, AppliedTypeTree(Ident(TypeName("TpeScopeRunner")), List(Ident(TypeName("R")))), id3))), termNames.CONSTRUCTOR), List(Ident(TermName("block"))))
        *
        */
        //def apply[R](b: => R) = new Scope[OptiWranglerLower, OptiWranglerLowerRunner[R], R](b)")
        //DefDef(Modifiers(), TermName("apply"), List(TypeDef(Modifiers(PARAM), TypeName("R"), List(), TypeBoundsTree(EmptyTree, EmptyTree))), List(List(ValDef(Modifiers(PARAM | BYNAMEPARAM/CAPTURED/COVARIANT), TermName("b"), AppliedTypeTree(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TypeName("<byname>")), List(Ident(TypeName("R")))), EmptyTree))), TypeTree(), Apply(Select(New(AppliedTypeTree(Ident(TypeName("Scope")), List(Ident(TypeName("OptiWranglerLower")), AppliedTypeTree(Ident(TypeName("OptiWranglerLowerRunner")), List(Ident(TypeName("R")))), Ident(TypeName("R"))))), termNames.CONSTRUCTOR), List(Ident(TermName("b")))))
        case DefDef(_, dslName, _, _, _, Apply(Select(New(AppliedTypeTree(Ident(TypeName("Scope")), List(identDSL, AppliedTypeTree(identDSLRunner, _), typeParam))), _), _)) =>
          dslScopes += Tuple2(dslName.toString, (identDSL, identDSLRunner, typeParam))
          q""

        case Apply(identTermName, List(body)) if dslScopes.contains(identTermName.toString) =>
          val (dsl, runner, typ) = dslScopes(identTermName.toString())
          val ret = q"""{
            trait DSLprog extends $dsl {def apply:$typ = $body }
            val cl = (new DSLprog with $runner[$typ]): $dsl with $runner[$typ]
            cl.apply
          }"""
          ret

        // this only works for: `new Scope[A, B, C]()` not for: `new Scope[A, B, C]{}` => creates anonymous class and stuff
        case Apply(Select(New(AppliedTypeTree(Ident(TypeName("Scope")), List(tn1, tn2, tnR))), termnames), List(body)) =>
          //TODO(trans): val bodyTranform = transform(body)
          val ret = q"""{
            trait DSLprog extends $tn1 {def apply = $body }
            val cl = (new DSLprog with $tn2): $tn1 with $tn2
            cl.apply
          }"""
          c.warning(tree.pos, s"SCOPE GENERATED: \n RAW: "+showRaw(ret)+"\n CODE: "+showCode(ret))
          ret

        case _ =>
          super.transform(tree)
      }
    }
    def apply(tree: c.universe.Tree): (List[Tree], Seq[DSLFeature]) =
      (transformStm(tree), lifted.toSeq)
  }
}
