package edu.colorado.csci3155.project1
import org.scalatest.FunSuite


class CompilerTest extends FunSuite {
                                                        /*           8 Test Cases that should all pass             */

    test ("simple expression 1") {                          // this test passed
        println("================================================")
        println(" Converting a CONST(v) into instruction test")
        val e = Const(1.0)
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        println("YOUR INSTRUCTION LIST: ", lst)
        assert(lst == List(PushI(1.0)))
    }

    test("simple expression 2") {                           // this test passed
        println("================================================")
        println(" Converting a PLUS(e1,e2) into instruction test")
        val e = Plus(Const(1.0), Const(2.5))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        println("YOUR INSTRUCTION LIST: ", lst)
        assert(lst == List(PushI(1.0), PushI(2.5), AddI))
    }

    test("minus test") {                                  // this test passed
        println("================================================")
        println(" Converting a MINUS(e1,e2) into instruction test")
        val e = Minus(Const(5.0), Const(3.0))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        println("YOUR INSTRUCTION LIST: ", lst)
        assert(lst == List(PushI(5.0),PushI(3.0), SubI))
    }

    test("division test") {                             // this test passed
        println("================================================")
        println(" Converting a Div(e1,e2) into instruction test")
        val e = Div(Const(15.0), Const(3.0))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        println("YOUR INSTRUCTION LIST: ", lst)
        assert(lst == List(PushI(15.0),PushI(3.0), DivI))
    }

    test("simple expression 3"){                        // this test passed
        println("================================================")
        println(" Converting a MINUS / PLUS / CONST into instruction test")
        val e = Minus(Plus(Const(1.5), Const(2.4)), Mult(Const(2.5), Const(2.5)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        //println("----")
        //lst.foreach(println)
        //println("----")
        assert(lst == List(PushI(1.5), PushI(2.4), AddI, PushI(2.5), PushI(2.5), MultI, SubI))
    }

    test("simple expression 4") {                      // this test passed
        println("================================================")
        println(" Converting a MINUS / PLUS / CONST / LOG / EXP into instruction test")
        val e1 = Minus(Plus(Const(1.5), Const(2.4)), Mult(Const(2.5), Const(2.5)))
        val e2 = Plus(Const(1.0), Const(2.5))
        val e3 = Log(Plus(Exp(e1), Exp(e2)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e3)
        //println("----")
        //lst.foreach(println)
        //println("----")
        val lst1 = List(PushI(1.5), PushI(2.4), AddI, PushI(2.5), PushI(2.5), MultI, SubI)
        val lst2 = List(PushI(1.0), PushI(2.5), AddI)
        val lst3 = lst1 ++ List(ExpI) ++ lst2 ++ List(ExpI) ++ List(AddI) ++ List(LogI)
        println("YOUR INSTRUCTION LIST: ", lst)
        assert(lst == lst3)
    }

    test("cosine expression") {                       // this test passed
        println("================================================")
        println(" Converting a COSINE into instruction test")
        val e1 = Cosine(Const(2.5))
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        //println("----")
        //lst.foreach(println)
        //println("----")
        val c1 = List(PushI(2.5), CosI)
        println("YOUR INSTRUCTION LIST: ", lst)
        assert( lst == c1 )
    }

    test("sine expression ") {                          // this test passed
        println("================================================")
        println(" Converting a SINE into instruction test")
        val e1 = Sine(Const(2.5))
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        //println("----")
        //lst.foreach(println)
        //println("----")
        val c1 = List(PushI(2.5), SinI)
        println("YOUR INSTRUCTION LIST: ", lst)
        assert( lst == c1 )
    }

}
