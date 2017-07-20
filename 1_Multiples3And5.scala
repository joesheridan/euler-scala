/*
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
 */

def isMultipleOf3Or5(num: Int): Boolean = {
  num % 3 == 0 || num % 5 ==0
}

// get multiples of 3 or 5 and add them up
(1 until 1000).filter(isMultipleOf3Or5(_)).sum