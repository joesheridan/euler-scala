/**
  *The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
  */

def isPalindromic(num: Int): Boolean = {
  num.toString() == num.toString().reverse
}

def isBinaryPalindromic(num: Int) : Boolean = {
  num.toBinaryString == num.toBinaryString.reverse
}

def isDoublePalindromic(num: Int):Boolean = {
  isPalindromic(num) && isBinaryPalindromic(num)
}

isPalindromic(585)
isPalindromic(586)

isBinaryPalindromic(585)
isBinaryPalindromic(686)

(1 until 1000000).filter(isDoublePalindromic(_)).sum