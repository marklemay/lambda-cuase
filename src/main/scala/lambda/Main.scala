package lambda

import LambdaParser._

//TODO: clean up a little
//TODO: make a spec system to test that partial evaluation is correct
// lit review
//if needed build a JS page to demo


object Main {
  def main(args: Array[String]): Unit = {


    val in = lam"({λA.A} {λA. A})"
    val out = in.eval

    println(in)
    println(out)

    val allOutPaths = out.leaves().flatMap(_.subPaths)

        val cuases = allOutPaths.map(p =>
          p -> { 
            Cuase.directCuase(_.eval)(in)(p) //print the cuase of every sub expression, in terms of a well behaved function (in this case eval)
          }).toMap
        println(cuases.mkString("\n"))

  }
}