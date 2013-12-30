(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1.sml";

val test1a = is_older((1,1,1),(1,1,1)) = false
val test1b = is_older((1,1,1),(2,1,1)) = true
val test1c = is_older((2,1,1),(1,1,1)) = false
val test1d = is_older((1,1,1),(1,2,1)) = true
val test1e = is_older((1,2,1),(1,1,1)) = false
val test1f = is_older((1,1,1),(1,1,2)) = true
val test1g = is_older((1,1,2),(1,1,1)) = false
val test1 = is_older((1,2,3),(2,3,4)) = true

val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1
val test2a = number_in_month([],2) = 0
val test2b = number_in_month([(2012,2,28),(2013,2,1)],2) = 2

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3a = number_in_months([],[2,3,4]) = 0
val test3b = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0 
val test3c = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[1,5,6]) = 0

val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4a = dates_in_month([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]
val test4b = dates_in_month([(2012,2,28),(2013,12,1)],1) = []
val test4c = dates_in_month([],2) = []

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5a = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5,6,9]) = []
val test5b = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val test5c = dates_in_months([],[5,6,9]) = []

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
val test6a = get_nth(["hi", "there", "how", "are", "you"], 1) = "hi"
val test6b = get_nth(["hi", "there", "how", "are", "you"], 5) = "you"

val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"
val test7a = date_to_string((2013, 1, 1)) = "January 1, 2013"
val test7b = date_to_string((2013, 12, 1)) = "December 1, 2013"

val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test8a = number_before_reaching_sum(5, [6,3,4]) = 0
val test8b = number_before_reaching_sum(6, [6,3,4]) = 0
val test8c = number_before_reaching_sum(7, [6,3,4]) = 1
val test8d = number_before_reaching_sum(70, [31,28,31,30,31,30,31,31,30,31,30,31]) = 2
val test8e = number_before_reaching_sum(15, [31,28,31,30,31,30,31,31,30,31,30,31]) = 0
val test8f = number_before_reaching_sum(31, [31,28,31,30,31,30,31,31,30,31,30,31]) = 0
val test8g = number_before_reaching_sum(32, [31,28,31,30,31,30,31,31,30,31,30,31]) = 1


val test9 = what_month(70) = 3
val test9a = what_month(15) = 1
val test9b = what_month(31) = 1
val test9c = what_month(32) = 2

val test10 = month_range(31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11a = oldest([]) = NONE


