(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["A","b","C"] = ["A","C"]
val test1b = only_capitals ["a","b","c"] = []
val test1c = only_capitals [] = []

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 [] = ""
val test2b = longest_string1 ["A","bc","de"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 [] = ""
val test3b = longest_string2 ["A","bc","de"] = "de"

val test4a= longest_string3 ["A","bc","C"] = "bc"
val test4aa = longest_string3 [] = ""
val test4ab = longest_string3 ["A","bc","de"] = "bc"

val test4b= longest_string4 ["A","B","C"] = "C"
val test4ba = longest_string4 [] = ""
val test4bb = longest_string4 ["A","bc","de"] = "de"

val test5 = longest_capitalized ["A","bc","C"] = "A";
val test5a = longest_capitalized ["A","bc","CD"] = "CD";
val test5b = longest_capitalized ["a","bc","cd"] = "";

val test6 = rev_string "abc" = "cba";
val test6a = rev_string "" = ""; 

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7a = (first_answer (fn x => if x > 6 then SOME x else NONE) [1,2,3,4,5] = 4) handle NoAnswer => true

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x < 5 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8b = all_answers (fn x => if x < 8 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]

val test9a = count_wildcards Wildcard = 1
val test9aa = count_wildcards (Variable "var") = 0
val test9ab = count_wildcards (TupleP [Wildcard, Variable "var", Wildcard]) = 2
val test9ac = count_wildcards (ConstructorP("cstr", Wildcard)) = 1
val test9ad = count_wildcards (ConstructorP("cstr", Variable "var")) = 0

val test9b = count_wild_and_variable_lengths (Variable "a") = 1
val test9ba = count_wild_and_variable_lengths (Variable "foo") = 3
val test9bb = count_wild_and_variable_lengths Wildcard = 1
val test9bc = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "foo", Variable "x"]) = 5
val test9bd = count_wild_and_variable_lengths (ConstructorP("cstr", (Variable "bar"))) = 3

val test9c = count_some_var ("x", Variable("x")) = 1;
val test9ca = count_some_var ("x", Wildcard) = 0;
val test9cb= count_some_var ("x", TupleP [Wildcard, Variable("x"), Variable("x")]) = 2;

(*
val test10 = get_var_list [] (Variable("x")) = ["x"]
val test10a = get_var_list [] (TupleP [Variable("x"), Variable("x")]) = ["x","x"]
*)

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP [Variable("x"), Variable("x")]) = false
val test10b = check_pat (TupleP [Variable("y"), Variable("x")]) = true

val test11_1a = match (Const(1), UnitP) = NONE
val test11_1b = match (Const(1), Wildcard) = SOME []
val test11_1c = match (Const(1), Variable("x")) = SOME [("x", Const(1))]
val test11_1d = match (Const(1), ConstP(1)) = SOME []
val test11_1e = match (Const(1), ConstP(2)) = NONE

val test11_2a = match (Unit, UnitP) = SOME []
val test11_2b = match (Unit, Wildcard) = SOME []
val test11_2c = match (Unit, Variable("x")) = SOME [("x", Unit)]

(*
val test11 = match (Const(1), Wildcard)) = SOME []
val test11 = match (Const(1), ConstP(1)) = SOME []
val test11a = match (Const(1), ConstP(2)) = NONE
val test11b = match (Const(1), ConstP(1)) = SOME []

val test11f = match (Constructor ("SOME", Const 42), ConstructorP ("SOME", Variable "x")) = SOME[("x", Const 42)]
val test11g = match (Constructor ("NONE", Unit), ConstructorP ("SOME", Variable "x")) = NONE
val test12 = first_match Unit [UnitP] = SOME []
*)

