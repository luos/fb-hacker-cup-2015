
import scala.collection.mutable.ArrayBuffer
	
def main(args: Array[String]): Unit = {
	  	val n = Num("1235223")
	  	tests()
	  	println( n.max )
	  	println( n.min )
	  	println( n.toList )

}


def tests() : Unit = {
	val n = Num("4322592163")
	assert( n.firstElem == 4 )
	assert( n.max == 9 )
	assert( n.min == 1 )

	assert( n.firstIndexOf( 2 )  == 2 )
	assert( n.lastIndexOf( 2 )  == 6 )

	assert( n.firstIndexOf( 4 )  == 0 )
	assert( n.firstIndexOf( 1 )  == 7 )


	val n2 = Num("345")
	assert( n2.toList == List( 3, 4, 5 ) )

	assert( n2.swapToMax == "543" )

	val n3 = Num( "54320" )
	assert( n3.swapToMax == "54320" )

	val nulla = Num( "0" )
	assert( nulla.swapToMax == "0" )

	val oneDigit = Num( "5" )
	assert( oneDigit.swapToMax == "5" )

	val twoDigit2 = Num( "33" )
	assert( twoDigit2.swapToMax == "33" )	

	val twoDigit = Num( "93" )
	assert( twoDigit.swapToMax == "93" )	

	tests2()
	exampleTests()

	println( "All " + testCount + " tests passed :-) " )

}

var testCount  = 0

def tests2() : Unit = {
	val threeDigit = Num( "439" )
	assert( threeDigit.swapToMax == "934" )
	assert( threeDigit.ascending == List( 3, 4 , 9 ) )
	assert( threeDigit.descending == List( 9, 4, 3 ) )

	val lotsDigits = Num("1342292341")
	assert( lotsDigits.swapToMax == "9342212341" )

	val n = Num( "912439" )
	assert( n.swapToMax == "992431" )

	val n2 = Num( "999999991" )
	assert( n2.swapToMax == "999999991" )

	val n3 = Num( "999999919" )
	assert( n3.swapToMax == "999999991" )

	val n4 = Num( "939919999" )
	assert( n4.swapToMax == "999919993" )
}

def exampleTests() : Unit = {
	val _31524 = Num( "31524" ) 
	assert( _31524.swapToMax == "51324" )

	val _897 = Num( "897" ) 
	assert( _897.swapToMax == "987" )

	val _123 = Num( "123" )
	assert( _123.swapToMax == "321" )


	val _10 = Num("10")
	assert( _10.swapToMax == "10")

	val _5 = Num("5")
	assert( _5.swapToMax == "5" )
}

def assert( b : Boolean ) : Unit = {
	testCount += 1
	if ( b ) {
		
	}else{
		throw new java.lang.Exception("You fud up")
	}
}

case class Num( n : String ){
		
		val toList = n.toList.map( a => Integer.parseInt( a.toString ) )
		val descending = toList.sortWith( _ > _ )
		val ascending = toList.sortWith( _ < _ )

		val max : Int = {
			toList.max
		}

		val min : Int = {
			toList.min
		}

		def lastIndexOf ( i : Int ) = {
			toList.lastIndexOf( i )
		}

		def firstIndexOf( i : Int ) = {
			toList.indexOf( i )
		}

		val firstElem : Int = toList( 0 )

		/**
		  * we get the max elem
		  * get the last index of the max elem
		  * and replace it with the first elem 
		  * if the first elem is the same as the max then we should do something
		  * then we should look at the second elem
		  *
		  */
		val swapToMax : String = {
			swawpMax
		}

		// 912439
		// 999991
		def swawpMax : String = {
			var orderedParts = descending
			var currentIndex = 0
			while( currentIndex < this.toList.length ) {
				if ( toList(currentIndex) == orderedParts(0) ){
					orderedParts = orderedParts.drop( 1 )
					currentIndex += 1
				} else {
					val parts = ArrayBuffer( toList : _* )
					val swappedElem = parts( currentIndex )
					parts(currentIndex) = orderedParts( 0 )
					val swappedToLastIndex = this.lastIndexOf( orderedParts( 0 ) )
					parts( swappedToLastIndex ) = swappedElem
					return parts.mkString
				}
			}
			return n;
		}
}


main( Array("a", "b") )