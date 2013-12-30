(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s, xs) = 
    let fun remove(s, xs) = 
            case xs of
                [] => [] 
                | x::xs' => if same_string(x, s) 
                            then remove(s, xs')
                            else x::remove(s, xs')
    in
        let val res = remove(s, xs)
        in
            if xs = res
            then NONE
            else SOME(res)
        end
    end

fun get_substitutions1(lst, s) =
    case lst of
        [] => []
        | xs::lst' => case all_except_option(s,xs) of
                        NONE => [] @ get_substitutions1(lst', s)
                        | SOME fs => fs @ get_substitutions1(lst', s)


fun get_substitutions2(lst, s) =
    let fun aux(lst, s, acc) = 
        case lst of 
            [] => acc
            | xs::lst' => case all_except_option(s,xs) of
                            NONE => aux(lst', s, [] @ acc)
                            | SOME fs => aux(lst', s, fs @ acc)
    in
        aux(lst, s, [])
    end 


fun similar_names(sublst, fullname) =
    let val {first=f, middle=m, last=l} = fullname
        val subs = get_substitutions2(sublst, f)
        fun makesub(xs) =
            case xs of
                [] => []
                | x::xs' => {first=x, middle=m, last=l}::makesub(xs')
    in
        fullname :: makesub(subs)
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
    
fun card_color(c) = 
    case c of 
        (Clubs,_) => Black
        | (Spades,_) => Black
        | (_,_) => Red

fun card_value(c) =
    case c of
        (_,Num(n)) => n
        | (_,r) => if r = Ace then 11 else 10

fun remove_card(cs, c, e) =
    case cs of
        [] => raise e 
        | (c'::cs') => if c' = c 
                       then cs'
                       else c'::remove_card(cs', c, e)

fun all_same_color(cs) =
    case cs of
        [] => true
        | _::[] => true
        | c1'::c2'::cs' => if card_color(c1') = card_color(c2')
                           then all_same_color(c2'::cs')
                           else false

fun sum_cards(cs) =
    let fun aux(cs, acc) =
        case cs of
            [] => acc
            | c::cs' => aux(cs', card_value(c) + acc) 
    in
        aux(cs, 0)
    end

fun score(cs, goal) =
    let val sum = sum_cards(cs) 
        fun prescore(goal) =
            if sum > goal
            then 3 * (sum - goal)
            else goal - sum
        val pre = prescore(goal)
    in
        if all_same_color(cs)
        then pre div 2
        else pre
    end

fun officiate(cs, ms, goal) =
    let fun aux(cs, ms, heldlst) =
            case ms of
                [] => heldlst
                | Discard(c)::ms' => aux(cs, ms', remove_card(heldlst, c, IllegalMove))
                | Draw::ms' => case cs of
                              [] => heldlst
                              | c'::cs' => if (card_value(c') + sum_cards(heldlst)) > goal
                                           then c'::heldlst
                                           else aux(cs', ms', c'::heldlst)
    in
        score(aux(cs, ms, []), goal)
    end

