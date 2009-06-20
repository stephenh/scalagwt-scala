object Test extends Application {
  def get[T](x: T) = { println("get: "+ x); x }

  // TESTS

  // re-order using names, call-site evaluation order
  test1(1, "@")
  test1(b = get("$"), a = get(2))
  test1(a = get(3), b = get("**")) // should not transform into a block. how to test?
  test3(b = get(110), a = get(11))(c = get("\\"), d = get(2.399))
  test3(get(14), get(3920))(d = get("}"), c = get("["))


  // mixing named and positional
  test1(get(4), b = get("@"))
  test2(get(8), v = get(9))(get("%"), l = get(5))
  test3(12, 13)("'", d = 16)


  // anonymous functions
  val f1: (Int, String) => Unit = test1(_, _); f1(6, "~")
  val f2: Int => Unit = test1(a = _, b = get("+")); f2(get(7))
  val f3 = test1(b = _: String, a = get(8)); f3(get("+"))
  val f4: (Int, String) => Unit = test1(_, b = _); f4(9, "?")

  val f5: Int => (String, Int) => Unit = test2(v = get(38), u = _)_
  f5(get(39))(get("|"), 10)

  val f6: (Double, String) => Unit = test3(get(13), _)(d = _, c = get("x"))
  f6(get(2.233), get("<"))


  test4(14)


  // defaults: subclass overrides, adds and inherits default
  val b = new Base
  b.test1(b = "nix")(982)(f = 0)
  val s = new Sub1
  s.test1(a = new { override def toString = "bla" })(m = 0)()

  // defaults are chosen dynamically
  val b2: Base = new Sub1
  b2.test1(b = "")(c = 93.3)(f = -1)



  // overloading resolution
  object t1 {
    def f(a: Int, b: String) = "first"
    def f(b: String, a: Int) = "second"
  }
  println(t1.f(1, "2")) // first

  object t2 {
    def f(a: Int, b: Double, c: Object) = "first"
    def f(a: Int, b: Double, c: String) = "second"
  }
  println(t2.f(1, c = new Base(), b = 2.2)) // first
  println(t2.f(28, b = 3.89, c = "ldksfj")) // second

  object t3 {
    def f(a1: Int) = "first"
    def f(a2: Int)(b: Int) = "second"
  }
  println(t3.f(a1 = 10))    // first
  println(t3.f(a2 = 20)(1)) // second

  object t4 {
    def f(a: Int, b: String = "foo") = "first"
    def f(a: Int) = "second"
  }
  println(t4.f(109)) // second
  println(t4.f(a = 20)) // second

  object t5 {
    def f(a: Object) = "first"
    val f: String => String = a => "second"
  }
  println(t5.f(new Sub1())) // firsst
  println(t5.f("dfklj"))    // second

  object t6 {
    def f(a: String = "sdf", b: Int) = "f"
    def f(a: Int, b: Int) = "s"
  }
  println(t6.f(b = 289)) // f

  object t7 {
    def f(a: Int, b: String*) = "first"
    def f(a: Int) = "second"
    def g(a: Sub1, b: Int*) = "third"
    def g(a: Base) = "fourth"
    def h(a: Base, b: Int*) = "fifth"
    def h(a: Sub1) = "sixth"
  }
  println(t7.f(1)) // second
  println(t7.f(a = 19)) // second
  println(t7.f(b = "sl19", a = 28)) // first
  println(t7.g(new Sub1(), 1, 2)) // third
  println(t7.g(new Base())) // fourth
  println(t7.h(new Base())) // fifth
  println(t7.h(new Sub1())) // sixth

  object t9 {
    def f(a: String, b: Int = 11) = "first"
    def f(a: Double) = "second"
  }
  println(t9.f("bla")) // first


  // vararg
  def test5(a: Int, b: Int)(c: Int, d: String*) = a +", "+ d.toList
  println(test5(b = 1, a = 2)(3, "4", "4", "4"))
  println(test5(b = 1, a = 2)(c = 29))


  // tuple conversion
  def foo(a: Int, b: Int)(c: (Int, String)) = a + c._1
  println(foo(b = 1, a = 2)(3, "4"))


  // by-name parameters
  def bn1(a: Int, b: => Int) = a
  println(bn1(b = get(10), a = get(11))) // should not see get(10)

  def bn2(a: Int, b: => Int)(c: Int = b) = a + b
  println(bn2(b = get(2), a = get(1))()) // should get: 1, 2, 2


  // constructors
  val a1 = new A(b = "dlkfj")(d = 102)
  println(a1.print)
  val a2 = new A[String, Nothing](2, "dkflj")(d = 2, c = "lskf")
  println(a2.print)
  val b1 = new B("dklfj")(e = "nixda")
  println(b1.printB)
  val c1 = new C(a = "dlkf", c = new { override def toString() = "struct" })(e = "???")
  println(c1.print)
  val c2 = C("dflkj", c = Some(209): Option[Int])(None, "!!")
  println(c2.print)
  val a_f: String => A[String, Nothing] = new A[String, Nothing](b = _)(d = 100)
  println(a_f("20").print)
  val c_f: Int => C[Int] = C("dlfkj", c = 10, b = _)(35, e = "dlkf")
  println(c_f(11).print)


  // "super" qualifier
  val b10 = new B1
  println(b10.bar())


  // defaults in traits / abstract classes
  val mn = new MN
  println(mn.foo()())
  println(mn.bar(10))
  // anonymous class
  println((new M { def foo[T >: String](x: Int, y: T)(z: String = "2") = z ; def bar(x: Int, y: Double) = x }).foo()())

  // copy method for case classes
  val fact = Factory(y = "blabla")()
  println(fact)
  println(fact.copy(x = -1)("dldl"))

  println(Fact2()("jyp"))
  println(Fact2(x = 1)())
  println(Fact2(10)().copy(y = "blabla")())


  // assignment to var <-> named argument
  var argName = 1
  test5(argName = (argName = 2))
  println(argName) // should be 2
  test5({argName = 3; ()})
  println(argName) // should be 3


  // dependent types and copy method
  val a11 = new A2
  val b11 = a11.B2(new a11.C2)(1)
  println(b11.copy()())



  // bug #2057
  class O { class I(val x: Int = 1) }
  class U extends O { val f = new I() }
  val u1 = new U
  println(u1.f.x)


  // names / defaults in self constructor call
  new A3("lskfdjlk")
  new A4(1.23, ",")


  // names / defaults in super constructor call
  new B4()
  new B5()

  // no re-naming of parameters which are free in a closure of the body (lambdalift)
  println(test6(10)())
  test7("jaa")

  // implicits + defaults
  {
    implicit val implInt = 10101
    println(test8())
  }

  println(test9)

  {
    implicit val implString = "blublu"
    println(test9)
  }


  // result type of default getters: parameter type, except if this one mentions any type
  // parameter, in which case the result type is inferred. examples:

  // result type of default getter is "String => String". if it were infered, the compiler
  // would put "Nothing => Nothing", which is useless
  def transform(s: String, f: String => String = identity _) = f(s)
  println(transform("my text"))

  // result type of the default getter is inferred (parameter type mentions type parameter T)
  def test10[T](x: List[T] = List(1,2)) = x
  println(test10())

  // some complicated type which mentions T
  def test11[T[P]](x: T[T[List[T[X forSome { type X }]]]] = List(1,2)) = x
  // (cannot call f using the default, List(1,2) doesn't match the param type)


  // DEFINITIONS
  def test1(a: Int, b: String) = println(a +": "+ b)
  def test2(u: Int, v: Int)(k: String, l: Int) = println(l +": "+ k +", "+ (u + v))

  def test3[T1, T2](a: Int, b: T1)(c: String, d: T2) = println(a +": "+ c +", "+ b +", "+ d)

  def test4(a: Int) = {
    def inner(b: Int = a, c: String) = println(b +": "+ c)
    inner(c = "/")
  }
  def test5(argName: Unit) = println("test5")
  def test6(x: Int) = { () => x }
  def test7(s: String) = List(1).foreach(_ => println(s))

  def test8(x: Int = 1)(implicit y: Int, z: String = "kldfj") = z + x + y
  def test9(implicit x: Int = 1, z: String = "klfj") = z + x
}


class Base {
  def test1[T1, T2](a: Int = 100, b: T1)(c: T2, d: String = a +": "+ b)(e: T2 = c, f: Int) =
    println(a +": "+ d +", "+ b +", "+ c +", "+ e +", "+ f)
}

class Sub1 extends Base {
  override def test1[U1, U2](b: Int, a: U1)(m: U2, r: String = "overridden")(o: U2, f: Int = 555) =
    println(b +": "+ r +", "+ a +", "+ m +", "+ o +", "+ f)
}


class A[T <: String, U](a: Int = 0, b: T)(c: String = b, d: Int) { def print = c + a + b + d }
class B[T](a: T, b: Int = 1)(c: T = a, e: String = "dklsf") extends A(5, e)("dlkd", 10) { def printB = super.print + e + a + b + c }

case class C[U](a: String, b: Int = 234, c: U)(d: U = c, e: String = "dlkfj") { def print = toString + d + e }


class A1 {
  def foo(a: Int = 10, b: String) = b + a
}
class B1 extends A1 {
  def bar(a: String = "dflk") = super.foo(b = a)
}

trait N {
  def foo[T >: String](x: Int = -1, y: T = "jupee")(z: String): String
}

abstract class M extends N {
  def foo[T >: String](x: Int, y: T)(z: String = "1"): String
  def bar(n: Int, m: Double = 1.239): Double
}

class MN extends M {
  def foo[T >: String](x: Int, y: T)(z: String) = z + x + y
  def bar(n: Int, m: Double) = n*m
}

case class Factory(x: Int = 1, y: String)(z: String = y)
case class Fact2[T, +U](x: T = "ju", y: U = 1)(z: T = 2)


// dependent types and copy method
class A2 {
  case class B2(x: C2)(y: Int) extends A2 {
    override def toString = "slkdfj" + y
  }
  class C2
}



// using names / defaults in self constructor call
class A3(x: String, y: Int = 10) {
  def this(a: Object) {
    this(y = 10, x = a.toString())
    println(x)
  }
}
class A4(x: String, y: Int = 11) {
  def this(b: Double, sep: String) {
    this(sep + b + sep)
    println(y)
  }
}


// using names / defaults in super constructor call
class A5(x: Int, val y: Int = 2)(z: Int = x + y)
class B4 extends A5(10)() {
  println(y)
}
class B5 extends A5(y = 20, x = 2)() {
  println(y)
}

// overriding default can be less specific (but has to conform to argument type!)
class A6 { def foo(a: Object = "dlkf") = 0 }
class B6 extends A6 { override def foo(a: Object = new Object) = 1 }
