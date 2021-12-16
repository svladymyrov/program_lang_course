(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter(fn s => Char.isUpper(String.sub(s,0)))

val longest_string1 = foldl (fn (s, acc) => if String.size s > String.size acc then s else acc) ""

val longest_string2 = foldl (fn (s, acc) => if String.size acc > String.size s then acc else s) ""

fun longest_string_helper f = foldl (fn (s, acc) => if f (String.size s, String.size acc) then s else acc) ""

val longest_string3 = longest_string_helper (fn (n1,n2) => n1>n2)

val longest_string4 = longest_string_helper (fn (n1,n2) => n1>=n2)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f = fn xs =>
	   case xs of
	       [] => raise NoAnswer
	     | x::xs' => case f x of SOME b => b 
				  | _ =>  first_answer f xs'

fun all_answers f = fn xs =>
		       let fun loop (ls, acc) =
			       case ls of
				   [] => acc
				 | x::ls' => case f x of
						 SOME l => loop(ls', acc @ l)
					       | _ => []
		       in
			   (* TODO: make it tail recursive *)
			   case loop(xs, []) of [] => NONE 
					      | x::rest => SOME (x::rest)
		       end

val count_wildcards = g (fn _ => 1) (fn x => 0)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x)
fun count_some_var (s, p) = g (fn _ => 0) (fn x => if x=s then 1 else 0) p
