package scala.lms
package epfl
package test3

trait Parsers { this: Matching with Extractors =>
  
  type Elem
  type Input = List[Elem]
  implicit val mE: Typ[Elem]
  //implicit val mI: Typ[List[Elem]]
  
  abstract class Parser {
    def apply(in: Rep[Input]): Rep[ParseResult]
  }

  abstract class ParseResult

  case class Success(rest: Input) extends ParseResult
  case class Failure() extends ParseResult


  object SuccessR {
    def apply(x: Rep[Input]): Rep[Success] = construct(classOf[Success], Success.apply, x)
    def unapply(x: Rep[Success]): Option[Rep[Input]] = deconstruct(classOf[Success], Success.unapply, x)
  }

  object FailureR {
    def apply(): Rep[Failure] = construct(classOf[Failure], (_: Unit) => Failure.apply(), unit(()))
    def unapply(x: Rep[Failure]): Boolean = deconstruct(classOf[Failure], { x:Failure => if (Failure.unapply(x)) Some(()) else None }, x).isDefined
  } // TODO: not so nice...

  object :!: {
    def apply[A:Typ](x: Rep[A], xs: Rep[List[A]]) = construct(classOf[::[A]], (::.apply[A] _).tupled, tuple(x, xs))
//    def unapply[A](x: Rep[::[A]]) = deconstruct2(classOf[::[A]], ::.unapply[A], x) // doesn't work: hd is private in :: !
    def unapply[A:Typ](x: Rep[::[A]]): Option[(Rep[A], Rep[List[A]])] = deconstruct2(classOf[::[A]], (x: ::[A]) => Some(x.head, x.tail), x)
  }


  def acceptElem(elem: Elem) = {
    new Parser {
      def apply(in: Rep[Input]) = in switch {
        case ((x: Rep[Elem]) :!: (rest: Rep[Input])) if x guard elem => // need types to make it compile
          SuccessR(rest)
      } orElse {
        case _ => FailureR()
      } end
    }
  }
  
  def seq(a: Parser, b: Parser) = {
    new Parser {
      def apply(in: Rep[Input]) = a(in) switch {
        case SuccessR(rest) => b(rest)
      } orElse {
        case _ => FailureR()
      } end
    }
  }
  
  def alt(a: Parser, b: Parser) = {
    new Parser {
      def apply(in: Rep[Input]) = a(in) switch {
        case s @ SuccessR(rest) => s
      } orElse {
        case _ => b(in)
      } end
    }
  }
  
//  def acceptElem(elem: Elem) = acceptIf(_ == elem)
  def acceptElems(elems: List[Elem]) = {
    elems.map(acceptElem).reduceLeft(seq)
  }
  
}