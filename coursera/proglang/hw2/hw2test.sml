(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val test1 = all_except_option("string", ["string"]) = SOME []
val test1a = all_except_option("foo", ["bar","baz"]) = NONE
val test1b = all_except_option("foo", ["foo","bar","baz"]) = SOME ["bar","baz"]
val test1c = all_except_option("foo", []) = NONE

val test2 = get_substitutions1([["foo"],["there"]], "foo") = []
val test2a = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test2b = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test3 = get_substitutions1([["foo"],["there"]], "foo") = []
val test3a = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test3b = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"},
	     {first="F", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}]

val test5 = card_color((Clubs, Num 2)) = Black
val test5a = card_color((Hearts, Num 2)) = Red
val test5b = card_color((Spades, Num 2)) = Black
val test5c = card_color((Diamonds, Num 2)) = Red

val test6 = card_value((Clubs, Num 2)) = 2
val test6a = card_value((Clubs, Ace)) = 11
val test6b = card_value((Clubs, King)) = 10
val test6c = card_value((Clubs, Queen)) = 10
val test6d = card_value((Clubs, Jack)) = 10

val test7 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7a = remove_card([(Hearts, Ace),(Clubs, King),(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs,King),(Hearts,Ace)]
(*val test7b = remove_card([(Hearts, Queen)], (Hearts, Ace), IllegalMove) handle IllegalMove => true *)

val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true
val test8a = all_same_color([]) = true
val test8b = all_same_color([(Hearts, Ace), (Clubs, Ace)]) = false 

val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
val test9a = sum_cards([]) = 0

val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10b = score([(Hearts, Num 4),(Clubs, Num 9)],10) = 9
val test10c = score([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test10d = score([(Hearts, Num 4),(Hearts, Num 9)],10) = 4

val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
             
