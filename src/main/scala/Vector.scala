package edu.uga.pickle

import scala.math.{sin, cos, atan, sqrt, min, max, abs}
import scala.annotation.tailrec

import edu.uga.pickle.body.Point
import edu.uga.pickle.application.Agent

package vector
{

	/** Provides a representation of Cartesian and Polar vectors for use in the [[golem]] simulator.	 
	 *
	 * @author: Terrance Medina, The University of Georgia
	 * @contact: medinat_at_uga.edu
	 * @date_created: 05 JAN 2015
	 * @lastupdated: 07 MAR 2015
	 *
	 */

/** Interface for Vectors */
abstract class Vector
{
	val priority: Int
	def enlarge(perimeter: Double): Vector

	def toCartesian: CartesianVector
	def toPolar: PolarVector

	/** Return the sum of this Vector and that Vector */
	def +(that: Vector) : Vector

	/** Return the prioritised Vector that is not null */
	def ##(that: Vector) : Vector =
	{

		val thisCart = this.toCartesian
		val thatCart = that.toCartesian
		if ( thisCart.x == 0 && thisCart.y == 0 ) that
		else if ( thatCart.x == 0 && thatCart.y == 0 ) this
		else
		{
			if( this.priority > that.priority ) this
			else that
		}
	}

	/** Return a Vector that is translated to an Agent's Point of View */
	def toPOV(a: Agent) : Vector

	/** Return a Vector with the back azimuth of this vector */
	def invert : Vector

	/** Returns the acute angle between two Vectors */
	def innerAngle(that: Vector): Double = 
	{
		//use the special Azimuth subtractor
		this.toPolar.azimuth - that.toPolar.azimuth
	}

	/** Returns true if this vector is to the left of that (i.e. the Agent's) vector */
	def isLeftOf(that: Vector): Boolean =
	{
		val normalized = ((that.toPolar.azimuth.degrees -	this.toPolar.azimuth.degrees) + 360 ) % 360
//		println("this: "+this)
//		println("that: "+that)
//		println("normalized = "+normalized)
		if (normalized == 180) true //TODO: make this randomized
		else if (normalized < 180) true 
		else false
	}

	/** Returns true if this vector is to the right of that (i.e. the Agent's) vector */
	def isRightOf(that: Vector): Boolean =
	{
		!(this isLeftOf that)
	}

	def toPov(that: Double): PolarVector =
	{
		new PolarVector(
			((this.toPolar.azimuth.degrees - that) + 360 ) % 360,
			this.toPolar.magnitude,
			this.priority
		)
	}

	def toPoint(a: Agent): Point = 
	{
		new Point(a.x+this.toCartesian.x, a.y+this.toCartesian.y)
	}
}

/** Represents a Vector as an x-offset and a y-offset from point (0,0).
 *
 * This provides an Agent-centric Vector, where the starting point is assumed
 * to be (0,0) and the Vector is the hypotenuse of the right triangle formed
 * by offsets x and y.
 * 
 * @param x
 * @param y
 *
 */
class CartesianVector(val x: Double, val y: Double, val priority: Int) extends Vector
{

	def this(x: Double, y: Double) = this(x, y, 0)
	val distanceSquared = x*x + y*y

	override def toString = "("+x+","+y+")"

	override def toCartesian: CartesianVector =
	{
		this
	}

	/** Return this Vector as a PolarVector with azimuth and magnitude. */
	override def toPolar: PolarVector = 
	{
		val azimuth = 
			if (x == 0 && y >= 0) 0.0
			else if (x == 0 && y < 0 ) 180.0
			else if (y == 0 && x > 0 ) 90.0
			else if (y == 0 && x < 0 ) 270.0
			//First quadrant
			else if (x > 0 && y > 0 ) atan(x/y).toDegrees
			//Second quadrant
			else if (x > 0 && y < 0) 180 + atan(x/y).toDegrees
			//Third quadrant
			else if (x < 0 && y < 0 ) 180 + atan(x/y).toDegrees
			//Fourth quadrant
			else if (x < 0 && y > 0 ) 360 + atan(x/y).toDegrees
			else 0

			new PolarVector(azimuth, sqrt(distanceSquared), this.priority)
	}

	override def toPOV(a: Agent): CartesianVector =
	{
		val thisPolar = this.toPolar
		(new PolarVector(thisPolar.magnitude, (a.rotation + thisPolar.azimuth.degrees) % 360)).toCartesian
	}

	/** Return a CartesianVector with magnitude approximately equal to perimeter */
	override def enlarge(perimeter: Double): CartesianVector =
	{
		this.toPolar.enlarge(perimeter).toCartesian
	}

	def adjust(desiredLength: Double): CartesianVector =
	{
//		val theta = this.toPolar.azimuth
//		new CartesianVector(cos(theta.radians) * desiredLength, sin(theta.radians) * desiredLength, this.priority)
		new PolarVector(this.toPolar.azimuth.degrees, desiredLength, 0).toCartesian
	}


	/** Return a CartesianVector that represents the sum of this Vector and that Vector. */
	def +(that: Vector) : Vector =
	{
		new CartesianVector( (this.toCartesian.x) + (that.toCartesian.x), (this.toCartesian.y) + (that.toCartesian.y), 0)
	}

	/** Return a CartesianVector that represents the back azimuth of this Vector. */
	def invert = new CartesianVector(this.x * -1, this.y * -1, this.priority)

  /** Return a CartesianVector with x and y offsets scaled by a given factor. */
	def scale(factor: Double) = new CartesianVector(this.x * factor, this.y * factor, this.priority)
	
}

/** Represents a Vector as an azimuth from 0\u00b0 and a magnitude. 
 *
 * This provides an Agent-centric Vector, where the starting point is assumed
 * to be (0,0) and the Vector is represented as an azimuth, or angle-offset
 * between [0\u00b0, 360\u00b0), and a magnitude.
 * 
 * @param az
 * @param magnitude
 *
 */
class PolarVector(az: Double, val magnitude: Double, val priority: Int) extends Vector
{

	def this(az: Double, magnitude: Double) = this(az, magnitude, 0)

	val azimuth = new Azimuth(az)

	override def toString = "("+azimuth.degrees+"\u00b0,"+magnitude+")"

	override def toPolar: PolarVector = 
	{
		this
	}

  /** Return a CartesianVector representation of this PolarVector */
	override def toCartesian: CartesianVector =
	{
		val azr = azimuth.radians
		val azd = azimuth.degrees

		if (azd == 0 ) new CartesianVector(0, magnitude, this.priority)
		else if (azd == 90 ) new CartesianVector(magnitude, 0, this.priority)
		else if (azd == 180 ) new CartesianVector(0, magnitude * -1, this.priority)
		else if (azd == 270 ) new CartesianVector(magnitude * -1, 0, this.priority)
		else if (azd > 0 && azd < 90 ) new CartesianVector(magnitude * sin(azr), magnitude * cos(azr) , this.priority)
		else if (azd > 90 && azd < 180 ) new CartesianVector(magnitude * sin((180).toRadians - azr), magnitude * cos((180).toRadians - azr) * -1, this.priority)
		else if (azd > 180 && azd < 270 ) new CartesianVector(magnitude * cos((270).toRadians - azr) * -1, magnitude * sin((270).toRadians - azr) * -1, this.priority)
		else if (azd > 270 && azd < 360) new CartesianVector(magnitude * sin((360).toRadians - azr) * -1, magnitude * cos((360).toRadians - azr), this.priority)
		else new CartesianVector(0, 0, 0)
	}

	override def toPOV(a: Agent): PolarVector =
	{
		new PolarVector(this.magnitude, (a.rotation + this.azimuth.degrees) % 360)
	}

	/** Return a PolarVector with magnitude approximately equal to perimeter */
	def enlarge(perimeter: Double): PolarVector =
	{
		new PolarVector(this.azimuth.degrees, perimeter, this.priority)
	}

	/** Return a PolarVector that represents the sum of this Vector and that Vector. */
	def +(that: Vector) : Vector =
	{
		new CartesianVector( (this.toCartesian.x) + (that.toCartesian.x), (this.toCartesian.y) + (that.toCartesian.y), 0).toPolar
	}

	//complement
	def invert = new PolarVector((this.azimuth.degrees + 180) % 360, this.magnitude, this.priority)
	

}

/** Represents the angle offset of a vector from 0\u00b0 as an angle [0\u00b0,360\u00b0). */
class Azimuth(val value: Double)
{
	/** Return the angle in degrees [0\u00b0,360\u00b0). */
	def degrees = value % 360
	/** Return the angle in radians. */
	def radians = degrees.toRadians

	/** Returns the difference of two angles as an acute angle between [0\u00b0,360\u00b0)
	 *	regardless of where the angles fall on the unit circle. 
	 */
	def -(that: Azimuth): Double =
	{
		val biggy = max(this.degrees, that.degrees)
		val tiny = min(this.degrees, that.degrees)

		val optionA = (360 - biggy) + tiny
		val optionB = 360 - optionA

		min(optionA, optionB)
	}
}

}
