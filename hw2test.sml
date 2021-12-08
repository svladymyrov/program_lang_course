(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw2.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test11a = all_except_option ("b", ["a", "d", "b", "c"]) = SOME ["a", "d", "c"]
val test12a = all_except_option ("b", ["a", "c"]) = NONE
val test13a = all_except_option ("b", []) = NONE
val test14a = all_except_option ("b", ["a", "c", "d", "b"]) = SOME ["a", "c", "d"]

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test22 = get_substitutions1 ([["foo", "bar"],["there"]], "foo") = ["bar"]
val test23 = get_substitutions1 ([["here"],["there"]], "foo") = []
val test24 = get_substitutions1 ([["foo", "buz"],["there"], ["foo", "bar", "buz"]], "foo") = ["buz", "bar", "buz"]
val test25 = get_substitutions1 ([["zoo", "boo"],["match","foo","dou"],["there"]], "foo") = ["match","dou"]


val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test33 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test34 = get_substitutions2 ([["foo", "bar"],["there"]], "foo") = ["bar"]
val test35 = get_substitutions2 ([["here"],["there"]], "foo") = []
val test36 = get_substitutions2 ([["foo", "buz"],["there"], ["foo", "bar", "buz"]], "foo") = ["buz","buz","bar"]
val test37 = get_substitutions2 ([["zoo", "boo"],["match","foo","dou"],["there"]], "foo") = ["dou","match"]


val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"},
	     {first="F", last="Smith", middle="W"},
	     {first="Fredrick", last="Smith", middle="W"}]
val test44 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Figu", middle="W", last="Smith"}) =
	    [{first="Figu", last="Smith", middle="W"}]


val test5 = card_color (Clubs, Num 2) = Black
val test55 = card_color (Spades, Num 2) = Black
val test56 = card_color (Hearts, Num 2) = Red
val test57 = card_color (Diamonds, Num 2) = Red


val test6 = card_value (Clubs, Num 2) = 2
val test66 = card_value (Hearts, Ace) = 11
val test67 = card_value (Clubs, King) = 10
val test68 = card_value (Diamonds, Queen) = 10
val test69 = card_value (Spades, Jack) = 10


val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test77 = remove_card ([(Hearts, Ace),(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]
val test78 = (remove_card ([(Diamonds, Jack)], (Hearts, Ace), IllegalMove) handle IllegalMove => []) = []


val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test80 = all_same_color [] = false
val test81 = all_same_color [(Hearts, Ace), (Spades, Queen)] = false
val test82 = all_same_color [(Spades, Ace), (Clubs, Jack), (Hearts, Num 7)] = false
val test83 = all_same_color [(Hearts, Ace)] = true


val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test90 = sum_cards [(Clubs, Ace),(Hearts, Jack),(Diamonds, Num 8)] = 29
val test91 = sum_cards [(Diamonds, Ace),(Clubs, Ace)] = 22
val test92 = sum_cards [] = 0
val test93 = sum_cards [(Clubs, Queen)] = 10


val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test100 = score ([(Hearts, Ace),(Clubs, Num 4)],10) = 15
val test101 = score ([(Hearts, Num 2),(Diamonds, Num 4),(Diamonds, Num 3)],10) = 0
val test102 = score ([],10) = 10


val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

(*
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)           
*)
