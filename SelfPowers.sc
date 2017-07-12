/*
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series,
1^1 + 2^2 + 3^3 + ... + 1000^1000.
 */

val sum = (1 to 1000).view.map(a => BigInt(a).pow(a)).
  map(a=> BigInt(a.toString.takeRight(10))).sum

  sum.toString.takeRight(10)
