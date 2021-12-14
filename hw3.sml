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

fun only_capitals ss = List.filter(fn s => Char.isUpper(String.sub(s,0))) ss

fun fold (f, acc, xs) =
    case xs of
	[] => acc
      | x::xs' => fold(f, f(acc, x), xs')

fun longest_string1 ss = fold((fn (acc, s) => if String.size s > String.size acc then s else acc), "", ss)

fun longest_string2 ss = fold((fn (acc, s) => if String.size acc > String.size s then acc else s), "", ss)

(*
fun longest_string_helper f = if f(1,2) then longest_string1 else longest_string2
val longest_string3 = longest_string_helper(fn (n1, n2) => n1>n2)
val longest_string4 = longest_string_helper(longest_string2)
*)
