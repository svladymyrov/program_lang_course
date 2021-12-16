(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["A","bbc","C"] = ["A","C"]
val test1b = only_capitals ["a"] = []
val test1c = only_capitals [] = []


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test20 = longest_string1 ["A","b","C"] = "A"
val test21 = longest_string1 [] = ""


val test3 = longest_string2 ["A","bc","C"] = "bc"
val test30 = longest_string2 ["A","b","C"] = "C"
val test31 = longest_string2 [] = ""


val test4a = longest_string3 ["A","bc","C"] = "bc"
val test40a = longest_string3 ["A","b","C"] = "A"

val test4b = longest_string4 ["A","B","C"] = "C"


val test5 = longest_capitalized ["A","bc","C"] = "A"
val test50 = longest_capitalized ["A","bc","Cx"] = "Cx"
val test51 = longest_capitalized ["a","bc","c"] = ""


val test6 = rev_string "abc" = "cba"
val test60 = rev_string "a" = "a"
val test61 = rev_string "" = ""


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test70 = (first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5] handle NoAnswer => 0) = 0


val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test80 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
val test81 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = NONE
val test82 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [2] = SOME [2]


val test9a = count_wildcards Wildcard = 1
val test90a = count_wildcards (TupleP [Wildcard,Wildcard]) = 2
val test91a = count_wildcards (TupleP [Wildcard,Wildcard,Variable "q"]) = 2


val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test90b = count_wild_and_variable_lengths (TupleP [Variable("a"),Wildcard,Variable("qwerty")]) = 8
val test91b = count_wild_and_variable_lengths (ConstructorP ("C",Variable "qw")) = 2


val test9c = count_some_var ("x", Variable("x")) = 1
val test90c = count_some_var ("zxc", (TupleP [Variable("a"),Wildcard,Variable("qwerty")])) = 0

(*
val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

*)
