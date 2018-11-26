object MyModule {
    def abs(n: Int): Int =
        if (n < 0) -n
        else n

    private def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    def factorial(n: Int): Int = {
        def go(n: Int, acc: Int): Int = 
        if (n <= 1) acc
        else go(n-1, acc * n)

        go(n, 1)
    }

    private def formatFactorial(x: Int) = {
        val msg = "The factorial value of %d is %d"
        msg.format(x, factorial(x))
    }

    def fib(n: Int): Int = {
        def go(n: Int, a: Int, b:Int): Int = {
            if (n <= 2) a + b
            else go(n-1, b, a+b)
        }

        go(n, 0, 1)
    }

    def formatResult(name: String, n: Int, f: Int => Int):String = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }

    def findFirst[A](ss: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int = 
            if(n >= ss.length) -1
            else if(p(ss(n))) n
            else loop(n+1)

        loop(0)
    }

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n1: Int, n2: Int): Boolean = 
            if(n2 >= as.length) true
            else if(!ordered(as(n1), as(n2))) false
            else loop(n2, n2+1)

        loop(0, 1)
    }

    def partiall[A,B,C](a: A, f: (A,B) => C): B => C =
        (b: B) => f(a, b)

    def curry[A,B,C](f: (A,B) => C): A => (B => C) =
        (a: A) => {
            (b: B) => f(a,b)
        }

    def main(args: Array[String]): Unit = {
        println(formatResult("factorial", 6, factorial))
        println(findFirst(Array(7,9,13), (x: Int) => x == 9))
        println(isSorted(Array(7,9,13), (x: Int, y: Int) => x <= y))
        val x = List(1,2,3,4,5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + sum(t)
            case _ => 101
        }
        println(x)
    }
}