(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* put your solutions for problem 1 here *)
fun all_except_option (s : string, lst : string list) =
    case lst of
	[] => NONE
        | x::lst' => if same_string(s, x)
		   then SOME lst'
		   else
		       case all_except_option(s, lst') of  
			   SOME l => SOME (x::l)
			 | _ => NONE
		       
fun get_substitutions1 (lst : string list list, s : string) =
    case lst of
	[] => []
     |  x::lst' => case all_except_option(s, x) of
		       SOME l => l@get_substitutions1(lst', s)
		     | NONE => get_substitutions1(lst', s)

fun get_substitutions2 (lst, s) =
    case lst of
	[] => []
      | x::lst' =>
	let
	    val substitutions = get_substitutions2(lst', s)
	    fun concat (lst1, lst2) =
		case lst1 of
		    [] => lst2
		  | x::rest => concat(rest, x::lst2)
	in
	    case  all_except_option(s, x) of
		SOME l => concat(l, substitutions)
	     |  NONE => substitutions
	end

fun similar_names (lst, r) =
    let
	val {first=fname, middle=mname, last=lname} = r
	fun make_result (names, acc) =
	    let val r::acc' = acc
	    in
		case names of
		    [] => acc
		  | x::rest => make_result(rest, r::({first=x,middle=mname,last=lname}::acc'))
	    end
    in
	make_result(get_substitutions2(lst, fname), [r])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (s, _) =
    case s of
	Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red

fun card_value (_, value) =
    case value of
	Num n => n
      | Ace => 11
      | _ => 10

fun remove_card (cs, c, ex) =
    case cs of
	[] => raise ex
     | x::rest => if x=c then rest else x::remove_card(rest, c, ex)

fun all_same_color cs =
    case cs of
	[] => false
      | x::[] => true
      | x::(y::rest) => if card_color(x)=card_color(y)
			then all_same_color(y::rest) else false

fun sum_cards cs =
    let	fun sum (cs, acc) =
	    case cs of
		[] => acc
	      | c::rest => sum(rest, card_value(c) + acc)
    in
	sum(cs, 0)
    end

fun score (cs, g) =
    let
	val cs_sum = sum_cards(cs)
	val is_same_color = all_same_color(cs)
	val preliminary_g = if cs_sum > 0 then (cs_sum - g) * 3 else cs_sum
	val preliminary_l = if cs_sum > 0 then g - cs_sum else cs_sum
	val dnum = 2
    in
	case cs_sum > g of
	    true => if is_same_color then preliminary_g div dnum else preliminary_g
	  | _ =>  if is_same_color then preliminary_l div dnum else preliminary_l
    end

fun officiate (cl, ml, g) =
    let
	fun process_moves (crds, mvs, hld_crds) =
	    case mvs of
		[] => score(hld_crds, g)
	      | (Discard c)::rest_mvs => process_moves(crds, rest_mvs, remove_card(hld_crds, c, IllegalMove))
	      | Draw::rest_mvs =>
		case crds of
		    [] => score(hld_crds, g)
		  | c::crds' => let val current_score = score(c::hld_crds, g)
				in
				    if current_score > g
				    then current_score
				    else process_moves(crds', rest_mvs, c::hld_crds)
				end
    in
	process_moves(cl, ml, [])
    end
