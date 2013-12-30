
fun is_older(d1 : (int * int * int), d2 : (int * int * int)) =
    if #1 d1 = #1 d2
    then 
        if #2 d1 = #2 d2
        then
            if #3 d1 = #3 d2
            then false
            else #3 d1 < #3 d2
        else #2 d1 < #2 d2
    else #1 d1 < #1 d2


fun number_in_month(ds: (int * int * int) list, m: int) = 
    if null ds
    then 0
    else 
        if #2 (hd ds) = m
        then 1 + number_in_month(tl ds, m)
        else 0 + number_in_month(tl ds, m)


fun number_in_months(ds: (int * int * int) list, ms: int list) = 
    if null ms
    then 0
    else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)


fun dates_in_month(ds: (int * int * int) list, m: int) =
    if null ds
    then []
    else
        if #2 (hd ds) = m
        then (hd ds) :: dates_in_month(tl ds, m)
        else dates_in_month(tl ds, m)


fun dates_in_months(ds: (int * int * int) list, ms: int list) =
    if null ms
    then []
    else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)


fun get_nth(xs: string list, n: int) = 
    let fun get_nth_rec(xs: string list, i: int) =
            if i = n
            then hd xs
            else get_nth_rec(tl xs, i + 1)
    in
        get_nth_rec(xs, 1)
    end


fun date_to_string(d: (int * int * int)) = 
    let val ms = ["January","Februrary","March","April","May","June","July","August","September","October","November","December"]
    in
        get_nth(ms, #2 d) ^  " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end


fun number_before_reaching_sum(sum: int, ns: int list) = 
    let fun index_sum(index: int, cur_sum: int, ns: int list) = 
        if cur_sum <= hd ns + hd (tl ns)
        then index
        else index_sum(index + 1, cur_sum - hd ns, tl ns)
    in
        if sum <= hd ns
        then 0
        else index_sum(1, sum, ns) 
    end


fun what_month(day: int) = 
    let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, days_in_months) + 1
    end


fun month_range(day1: int, day2: int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)


fun oldest(ds: (int * int * int) list) = 
    let fun find_oldest(cur_old: (int * int * int), remaining_ds: (int * int * int) list) = 
            if null remaining_ds
            then cur_old
            else 
                if is_older(hd remaining_ds, cur_old)
                then find_oldest(hd remaining_ds, tl remaining_ds)
                else find_oldest(cur_old, tl remaining_ds)
    in
        if null ds
        then NONE
        else SOME(find_oldest(hd ds, tl ds))
    end




