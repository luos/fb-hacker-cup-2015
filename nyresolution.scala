
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.collection.mutable.ArraySeq

def main(args: Array[String]): Unit = {
	tests()
	solveInput("new_years_resolution_example_input.txt")


}

def solveInput( fileName : String ) = {
	var lines = scala.io.Source.fromFile( fileName ).getLines.toList
	val output = new PrintWriter( "nyresolution_output.txt" )
	var caseNum = 1
	var taskCount = Integer.parseInt( lines(0) )
	var tasks = new ArrayBuffer[( Nutrients, Seq[Nutrients] )]
	var currentLine = 1
	while( taskCount > 0 ){
		val goalParams = lines( currentLine ).split(" ").map( Integer.parseInt ).toList
		val goal = Nutrients( goalParams( 0 ), goalParams( 1 ), goalParams( 2 ) )
		currentLine += 1
		var foodCount = Integer.parseInt( lines( currentLine ) )
		val foods = new ArrayBuffer[Nutrients]()
		currentLine += 1
		while( foodCount > 0 ){
			val foodParams = lines( currentLine ).split( " " ).map( Integer.parseInt ).toList
			val food = Nutrients( foodParams( 0 ), foodParams( 1 ), foodParams( 2 ) )
			foods += food
			foodCount += -1
			currentLine += 1
		}
		val foodList : Seq[Nutrients] = ArraySeq( foods : _* )
		val elem =  ( goal, foodList )
		tasks += elem
		taskCount -= 1;
	}

	var caseCount = 1
	tasks.foreach( task => {
		val ( goal, foods ) = task
		val can = canEatToday( goal, foods )
		val result = s"Case $caseCount: " + ( if ( can ) "yes" else "no" )
		println( result )
		output.println( result )
		caseCount += 1
	})
	output.close()
}


def test1(){
   val goal = Nutrients( 100, 100, 100 )
   val foods = List(
   		Nutrients( 50, 94, 12 ),
   		Nutrients( 30 , 30, 30 ),
   		Nutrients( 15 , 15, 10 ),
   		Nutrients( 30 , 30, 30 ),
   		Nutrients( 15 , 45, 20 ),
   		Nutrients( 40 , 40, 40 ),
   		Nutrients( 50, 94, 12 )

   	)
   assert( canEatToday( goal , foods ) )
}
def tests() : Unit = {
	val n1 = Nutrients( 100 , 100 , 100 )
	val n2 = Nutrients( 10, 10, 99 )
	val n3 = Nutrients( 100 , 100 , 101 )
	val n4 = Nutrients( 200 , 200 , 101 )

	assert( n2.isLessThan( n1 ) )
	assert( n1.isLessThan( n3 ) )

	test1()
	impossible()
	println( "All " + testCount + " tests passed :-) " )
}

def impossible(){
	val goal = Nutrients( 3, 3, 3 )
	val foods = List(
   		Nutrients( 50, 94, 12 ),
   		Nutrients( 30 , 30, 30 ),
   		Nutrients( 15 , 15, 10 ),
   		Nutrients( 30 , 30, 30 ),
   		Nutrients( 15 , 45, 20 ),
   		Nutrients( 40 , 40, 40 ),
   		Nutrients( 50, 94, 12 )
   	)
   	assert( ! canEatToday( goal , foods ) )
}


var testCount = 0
def assert( b : Boolean ) : Unit = {
	testCount += 1
	if ( b ) {
		
	}else{
		throw new java.lang.Exception("You fud up")
	}
}

def canEatToday( goal : Nutrients, foods : Seq[Nutrients] ) : Boolean = {
	canEatMore( goal, Nutrients( 0, 0, 0 ), foods )
}

def canEatMore( goal: Nutrients, current : Nutrients,  availableFoods : Seq[Nutrients] ) : Boolean = {
	if ( goal == current ){
		return true;
	}
	
	val edibleFoods = availableFoods.map( food => {
		val ifEaten = current.add( food )
		if ( ifEaten.isLessThan( goal ) ){
			val availableF = availableFoods.filter( f => !(f.eq(food)) )
			canEatMore( goal, ifEaten, availableF )
		}else{
			ifEaten == goal
		}
	})
	edibleFoods.contains( true )	
}



case class Nutrients( protein : Int, carbohydrates : Int, fat : Int ){
	
	def add( n : Nutrients ) : Nutrients = {
		Nutrients( n.protein + this.protein, n.carbohydrates + this.carbohydrates, n.fat + this.fat )
	}

	def isLessThan( goal : Nutrients) : Boolean = {
		this.protein < goal.protein || this.carbohydrates < goal.carbohydrates || this.fat < goal.fat
	}

}









main( Array( "a" ) )
