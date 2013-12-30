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
fun only_capitals ls =
	List.filter (fn s => Char.isUpper(String.sub(s,0))) ls

fun longest_string1 ls = 
	List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" ls

fun longest_string2 ls = 
	List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" ls

fun longest_string_helper f ls =
	List.foldl f "" ls

val longest_string3 = longest_string_helper (fn (x,y) => if String.size(x) > String.size(y) then x else y)

val longest_string4 = longest_string_helper (fn (x,y) => if String.size(x) >= String.size(y) then x else y)

val longest_capitalized = (longest_string1 o only_capitals)

val rev_string = (String.implode o rev o String.explode)

fun first_answer f xs =
	case xs of 
		[] => raise NoAnswer
		| x::xs' => case f x of
						SOME(i) => i  
						| NONE => first_answer f xs' 

fun all_answers f xs =
	let fun helper f acc ys = 
			case ys of 
				[] => acc
				| x::ys' => case f x of
							   SOME(ls) => helper f (acc @ ls) ys'
							   | NONE => []
	in
		case (helper f [] xs) of [] => NONE | zs => SOME(zs)
	end

fun count_wildcards p = 
	g (fn () => 1) (fn x => 0) p	

fun count_wild_and_variable_lengths p = 
	g (fn () => 1) (String.size) p

fun count_some_var(s, p) =
	g (fn () => 0) (fn v => if v = s then 1 else 0) p

fun check_pat p = 
	let
	fun get_var_list acc p =
		case p of
			Wildcard => acc
			| Variable x => [x]
			| TupleP ps => List.foldl (fn (x,y) => (get_var_list acc x) @ y) acc ps
			| ConstructorP(_,p) => get_var_list acc p
			| _ => acc

	fun has_repeats xs =
		case xs of
			[] => false
			| x::xs' => List.exists (fn y => y = x) xs' orelse has_repeats xs'
	in
		not (has_repeats (get_var_list [] p))
	end



fun match(v, p) =
	case p of
		Wildcard => SOME []
		| Variable(s) => SOME [(s,v)]
		| UnitP => case v of Unit => SOME [] 
		| _ => NONE
