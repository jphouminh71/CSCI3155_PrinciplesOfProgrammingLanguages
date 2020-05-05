package edu.colorado.csci3155.project1

import org.scalatest.FunSuite

class StackMachineTest extends FunSuite {

    /* 10 test cases that should pass */

    test("stack machine test 1") {      // this test passed
        println("======================================================")
        println("ADDITION TEST: ")
        val lst1 = List(PushI(2.5), PushI(3.5), AddI)
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        assert(f == 6.0)
    }

    test( testName = "logarithm test"){             // this test passed
        println("======================================================")
        println("LOGARITHM TEST: ")
        val lst = List(PushI(100.0),LogI)
        val f = StackMachineEmulator.emulateStackMachine(lst)
        assert(f ==  scala.math.log(100.0))
    }

        test( testName = "exponent test"){          // this test passed
        println("======================================================")
        println("EXPONENT TEST: ")
        val lst = List(PushI(4.0),ExpI)
        val f = StackMachineEmulator.emulateStackMachine(lst)
        val shouldBe = scala.math.exp(4.0)
        assert(f <=  shouldBe)
    }

    test( testName = "multiplication test"){         // this test passed
        println("======================================================")
        println("MULTIPLICATION TEST: ")
        val lst = List(PushI(5.0),PushI(3.0),MultI)
        val f = StackMachineEmulator.emulateStackMachine(lst)
        assert(f ==  15.0)
    }

    test( testName = "division test"){                      // this test passed
        println("======================================================")
        println("DIVISION TEST: ")
        val lst = List(PushI(25.0),PushI(5.0),DivI)
        val f = StackMachineEmulator.emulateStackMachine(lst)
        assert(f ==  5.0)
    }

    test( testName = "subtraction test"){               // this test passed
        println("======================================================")
        println("SUBTRACTION TEST: ")
        val lst = List(PushI(5.0),PushI(3.0),SubI)
        val f = StackMachineEmulator.emulateStackMachine(lst)
        assert(f ==  2.0)
    }

    test("stack machine test 2") {              // this test passed
        println("======================================================")
        println("ADDITION / EXPONENT / LOGARITHM TEST: ")
        val lst1 = List(PushI(2.5), PushI(3.5), AddI, ExpI, LogI)
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        assert(math.abs(f - 6.0) <= 1e-05)
    }

    test("stack machine test 5") {              // this test passed
        println("======================================================")
        println("ARITHMETIC TEST: ")
        val lst1 = List(PushI(1.5), PushI(2.4), AddI, PushI(2.5), PushI(2.5), MultI, SubI)
        val lst2 = List(PushI(1.0), PushI(2.5), AddI)
        val lst3 = lst1 ++ List(ExpI) ++ lst2 ++ List(ExpI) ++ List(AddI) ++ List(LogI)
        val f = StackMachineEmulator.emulateStackMachine(lst3)
        assert(math.abs(f - 3.50287) <= 1E-04)
    }

    test ( testName = "sine test"){                         // this test passed
        println("======================================================")
        println("SINE TEST: ")
        val lst1 = List(PushI(4.88), SinI)
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        val shouldBe = scala.math.sin(4.88)
        assert( f == shouldBe)
    }

    test ( testName = "cosine test"){                       // this test passed
        println("======================================================")
        println("COSINE TEST: ")
        val lst1 = List(PushI(4.88), CosI)
        val f = StackMachineEmulator.emulateStackMachine(lst1)
        val shouldBe = scala.math.cos(4.88)
        assert( f == shouldBe)
    }

    /*        3 Test cases that should fail               */

    test( testName = "divide by zero"){
        println("======================================================")
        println("DIVIDE BY ZERO: ")
        val lst = List(PushI(25.0),PushI(0.0),DivI)
        //val f = StackMachineEmulator.emulateStackMachine(lst)
        assertThrows[IllegalArgumentException](StackMachineEmulator.emulateStackMachine(lst))

    }

    test( testName = "bad logarithm test"){
        println("======================================================")
        println("BAD LOGARITHM TEST: ")
        val lst = List(PushI(-1),LogI)
        //val f = StackMachineEmulator.emulateStackMachine(lst)
        assertThrows[IllegalArgumentException](StackMachineEmulator.emulateStackMachine(lst))
    }

    // test case that will result in an empty stack error
    test("empty stack test") {
        println("======================================================")
        println("INVALID AMOUNT OF ARGUMENT TEST: ")
        val lst = List( PushI(3.5), AddI)
        //val f = StackMachineEmulator.emulateStackMachine(lst1)
        //assert(f == 6.0)
        assertThrows[IllegalArgumentException](StackMachineEmulator.emulateStackMachine(lst))
    }
}
