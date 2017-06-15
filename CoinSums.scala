/*
  * In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
  */
case class Combination(pennies: Int, twopence: Int, fivepence: Int, twentypence: Int, fiftypence: Int, pounds: Int, twopounds: Int)

def sumCoins(combo: Combination): Int = {
  combo.pennies * 1 + combo.twopence * 2 + combo.fivepence * 5 + combo.twentypence * 20 +
    combo.fiftypence * 50 + combo.pounds * 100 + combo.twopounds * 200
}

// need to use iterators in the for loop since without, all memory is consumed
val sums = for (pennies <- (0 to 200).iterator; twopence <- (0 to 100).iterator; fivepence <- (0 to 40).iterator;
                twentypence <- (0 to 10).iterator;
     fiftypence <- (0 to 4).iterator; pounds <- (0 to 2).iterator; twopounds <- (0 to 1).iterator) yield {
  Combination(pennies, twopence, fivepence, twentypence, fiftypence, pounds, twopounds)
}

println(sums.filter(sumCoins(_) == 200).length)
