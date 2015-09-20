package edu.uga.pickle.evolve

import scala.math.pow

class Calculator
{
	def factorial(i: Int): Double = 
	{
		def doit(remaining: Int, acc: Double): Double =
		{
			if( remaining == 0 ) acc
			else doit(remaining -1, acc*remaining)
		}

		doit(i, 1)
	}

	def choose(n:Int, r:Int): Double =
	{
//				println("fac(n) = "+factorial(n))
//				println("fac(r) = "+factorial(r))
//				println("fac(n-r) = "+factorial(n - r))
				if( n < r ) return 0
				val nList = (for( i <- 1 until n+1 ) yield i).toSet
				val rList = for( i <- 1 until r+1 ) yield i
				val nrList = (for( i <- 1 until (n- r)+1 ) yield i).toSet

				val top = nList.diff(nrList)
				println(top)
				val numerator:Double = top.foldLeft(1.0)( (x:Double, y:Int) => x * y )
				println(numerator)
				
				numerator/factorial(r)
//				factorial(n)/( factorial(r) * factorial(n - r)	)
	}

	def permute(n:Int, r:Int): Double =
	{
				factorial(n)/factorial(n - r)	
	}

	def sum(xs: List[Double]): Double = (0.0/:xs)( _ + _ )

	def priorityCOs(sensors: Int): Double =
	{
		val ps = (for( i <- 1 until sensors+1 ) yield permute(sensors, i)).toList
		for( p <- ps ) println(p)
		sum(ps)
	}

	def sumCOs(sensors: Int): Double =
	{
		val ps = (for( i <- 1 until sensors+1 ) yield choose(sensors, i) * pow(3,i) * pow(10,i) ).toList
		for( p <- ps ) println(p)
		sum(ps)
	}

}

object SearchSpace
{
	def main(args: Array[String])
	{
		val calc = new Calculator()
		val possiblePCOs = calc.priorityCOs(5)
		val possibleSCOs = calc.sumCOs(5)
		println("SCOs = "+possibleSCOs);
		println("PCOs = "+possiblePCOs);
		println("C(1890,5) = "+calc.choose(1890,5))
		println("fac(630) = "+calc.factorial(630))
	}
}
