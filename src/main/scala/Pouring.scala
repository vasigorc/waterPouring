package week2
/**
  * as a parameter takes all the glasses and
  * their capacities
  * Created by vgorcinschi on 07/03/17.
  */
class Pouring (capacity: Vector[Int]){

  // States

  type State = Vector[Int]
  // hard set all glasses to 0
  val initialState = capacity map (x => 0)

  //Moves

  trait Move {
    //we need to record how moves changes state
    def change(state: State):State
  }
  case class Empty(glass: Int) extends Move {
    /*
    an updated (at some point) old state is yielded. At which point?
    At the point of the passed-in state.
    'updated' just yields a new state, the old one will still be available...
     */
    override def change(state: State): State = state updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    //similar as above, but instead of 0 it is full capacity
    override def change(state: State): State = state updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    //we need to record how moves changes state
    override def change(state: State): State = {
      //first define the amount that gets poured from one glass to the other
      //how much water is in from vs. how much water is currently in  to
      val amount = state(from) min (capacity(to) - state(to))
      /*
      it is the old state updated at the 'from' glass, where the new value of the 'from' glass
      is the 'from - amount'. The new value of the 'to' glass is the previous value of the to glass
      plus the amount
       */
      state updated(from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  //all glasses
  val glasses = 0 until capacity.length

  /*
  first moves are emptying the glasses - we'll use a for-comprehension
  other moves - filling all the glasses
  last is the move from pouring from all glasses to other (+we cannot draw from one glass into the same)
  * */
  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
  (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  /*
   Paths (sequences of moves)
   The history is taken in reverse order, making thus easy to prepend new moves to the path
    */
  class Path(history: List[Move], val endState: State){
    //what does this path lead to. To get to it we will pattern match on the history
    //to avoid constant re-calculations we comment even this one and add endState as a constr param
    //def endState: State = (history foldRight initialState) (_ change _)

    //UNUSED FUNCTION: track the remainder of the list and in the end change the result of that
    //so this is a foldRight. Thus the refacting from above
    private def trackState(xs: List[Move]):State = xs match {
      case Nil => initialState
      case move::xs1 => move change trackState(xs1)
    }

    //extend the path  ++++ endState will be the added move changing the previous state
    def extend(move: Move) = new Path(move::history, move change endState)
    //print objects in an intelligible AND chronological manner
    override def toString = (history.reverse mkString " ") + "-->"+ endState
    //initial path
  }

  val initialPath = new Path(Nil, initialState)
  /*
  Now we need a way to progressively increase the moves on the paths
  it is simillar to the from() method from the streams worksheet, only that here instead of
  the integers we take paths.. In order to not revisit the same states over and over again
  we have a second parameter 'explored'
   */
  def from(paths: Set[Path], explored: Set[State]):Stream[Set[Path]] =
    if(paths isEmpty) Stream.empty
    else {//here's the gist of the exercise where we generate the
      //new 'paths' more
      val more = for{
                path <- paths//for each of the path
                next <- moves map path.extend//we generate a next path by extending the current path
        if !(explored contains(next.endState)) //do not double copy a path with the same endState!!!
        //to each possible move we apply the operation extend with 'this' move to the path this yields a new path
      } yield next //all those more path will yield a more set
      paths #:: from(more, explored ++ (more map (_.endState))) //start with paths then more...
    }

  //define the set of all paths
  val pathSets = from(Set(initialPath), Set(initialState))//first iteration - paths of lentgh one, second - of length two and etc to infinity

  //return stream of paths containing the target
  def solutions(target: Int):Stream[Path] = for{
    set <- pathSets
     path <- set
      if path.endState.contains(target)
  } yield path
}
