(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1111 = is_older ((1956,12,31),(1956,12,31)) = false
val test12 = is_older((2011,3,31),(2011,4,28)) = true
val test13 = is_older((2011,4,28),(2011,3,31)) = false
				     

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]


val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

(*
val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
*)


val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test81 = number_before_reaching_sum (10, [10,2,3,4,5]) = 0
(* val test82 = number_before_reaching_sum (10, [1,1,1,1,1]) = Empty *)


val test9 = what_month 70 = 3
val test91 = what_month 59 = 2
val test92 = what_month 365 = 12
val test93 = what_month 7 = 1


val test10 = month_range (31, 34) = [1,2,2,2]
val test101 = month_range (34, 66) = [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3]
val test102 = month_range (66, 59) = [3,2]
val test103 = month_range (92, 92) = [4,4]


val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test111 = oldest([(2020,1,1),(2020,1,1)]) = SOME (2020,1,1)
val test112 = oldest([]) = NONE
val test113 = oldest([(2021,11,23)]) = SOME (2021,11,23)

