/** An implementation of "unboxed Option", with boxing on the "None" case.
 *
 *  The problem being addressed is that of boxing, or the allocation of new objects to wrap
 *  existing values, as the existing cost of using `scala.Option`.  A value `Option[A]` has only two
 *  interesting cases:
 *
 *  - the initial value `A`, or
 *  - the lack of one such value
 *
 *  The `Some` objects that wraps `A` values are an unfortunate cost.
 *
 *  Commonly `null` is used as the other value, however, asides from the `NullPointerException`s
 *  that result in using `null`, `null` is also limited in that as it's not possible to distinguish
 *  between:
 *
 *  - a `null: Option[A]`, and
 *  - a `null: Option[Option[A]]`
 *
 *  ... which for some is not an acceptable loss.
 *
 *  The implementation here uses boxing (also referred to as "tagging") on the missing or `None`
 *  case, so the layers may be distinguished.  So the equivalent of `null: Option[A]` is
 *  `NULL: Opt[A]` and for `null: Option[Option[A]]` `Opt(NULL)`.  Using 3 layers,
 *  `Option[Option[Option[A]]]` vs `Opt[Opt[Opt[A]]]`, here are the possible values:
 *
 *  {{{
 *  None
 *  Some(None)
 *  Some(Some(None)))
 *  Some(Some(Some(A)))
 *
 *  NULL
 *  Opt(NULL)
 *  Opt(Opt(NULL))
 *  A
 *  }}}
 *
 *  Additionally, the NULL wrappers are memoized (in `SoftReference`s so they don't leak memory).
 *
 *  You may have noticed a caveat from the example above: `toString` on an `Opt[Opt[Opt[A]]]` is
 *  indistinguishable from a bare `A` value or any other number of layers.  The same goes for
 *  `equals` and `hashCode` (indeed all the methods that are defined on the `AnyRef`, aka `Object`,
 *  being the super-type shared between `A` and `Opt[A]`).  Similarly `case Opt(x)` will match
 *  any non-empty `Opt` (but you may use an inner pattern, e.g. `Opt(x: String)`.  Also, due to
 *  `Opt.unapply` being a custom unapply, match exhaustivity isn't present when pattern matching a
 *  `Opt`.
 */
package object libopt {
  type Opt[+A] >: NULL <: AnyRef

  val NULL: NULL = new NULL()

  object Opt {
    def apply[A](x: A): Opt[A] = x match {
      case null    => NULL
      case x: NULL => x.wrap
      case _       => x.asInstanceOf[Opt[A]]
    }

    def unapply[A](opt: Opt[A]): Opt.Ops[A] = new Opt.Ops(opt)

    implicit final class Ops[A](private val self: Opt[A]) extends AnyVal {
      def isEmpty: Boolean   = self eq None
      def nonEmpty: Boolean  = !isEmpty
      def isDefined: Boolean = nonEmpty

      @throws[NoSuchElementException]
      def get: A = self match {
        case x: NULL => x.unwrap
        case _       => self.asInstanceOf[A]
      }

      def cata[B](alt: => B, f: A => B): B              = if (isEmpty) alt else f(get)
      def fold[B](alt: => B)(f: A => B): B              = cata(alt, f)
      def getOrElse[X >: A](alt: => X): X               = cata(alt, x => x)
      def orElse[X >: A](alt: => Opt[X]): Opt[X]        = cata(alt, Opt(_))
      def foreach(f: A => Unit): Unit                   = cata((), f)
      def exists(p: A => Boolean): Boolean              = cata(false, p)
      def forall(p: A => Boolean): Boolean              = cata(true, p)
      def flatMap[B](f: A => Opt[B]): Opt[B]            = cata(NULL, f)
      def contains[X >: A](x: X): Boolean               = exists(_ == x)
      def orNull[X >: A](implicit z: Null <:< X): X     = getOrElse(z(null))
      def flatten[B](implicit z: A <:< Opt[B]): Opt[B]  = flatMap(z)
      def map[B](f: A => B): Opt[B]                     = flatMap(x => Opt(f(x)))
      def filter(p: A => Boolean): Opt[A]               = flatMap(x => if (p(x)) Opt(x) else NULL)
      def withFilter(p: A => Boolean): Opt[A]           = filter(p)
      def collect[B](pf: PartialFunction[A, B]): Opt[B] = map(pf.orElse(_ => null.asInstanceOf[B]))
      def iterator: Iterator[A]                         = cata(Iterator.empty, Iterator.single(_))
      def toList: List[A]                               = cata(Nil, _ :: Nil)
      def toRight[X](left: => X): Either[X, A]          = cata(Left(left), Right(_))
      def toLeft[X](right: => X): Either[A, X]          = cata(Right(right), Left(_))
      def toOption: Option[A]                           = cata(scala.None, scala.Some(_))
    }
  }

  final class NULL(private val value: NULL) {
    private var next = new java.lang.ref.SoftReference[NULL](null)
    def this() = this(null)

    @throws[NoSuchElementException]
    def unwrap[A]: A =
      if (value == null) throw new NullPointerException("NULL.unwrap")
      else value.asInstanceOf[A]

    def wrap: NULL = {
      var wrap = next.get
      if (wrap == null) {
        wrap = new NULL(this)
        next = new java.lang.ref.SoftReference(wrap)
      }
      wrap
    }

    override def equals(x: Any): Boolean = {
      (this eq x.asInstanceOf[AnyRef]) || {
        x match {
          case that: NULL => value == that.value
          case _          => false
        }
      }
    }

    override def hashCode(): Int  = value.##
    override def toString: String = if (value == null) "NULL" else s"Opt($value)"
  }
}
