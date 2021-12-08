fun is_older (d1 : int*int*int, d2 : int*int*int) =
    if (#1 d1)=(#1 d2) andalso (#2 d1)=(#2 d2) then (#3 d1) < (#3 d2)
    else if (#1 d1)=(#1 d2) then (#2 d1) < (#2 d2)
    else (#1 d1) < (#1 d2);

fun number_in_month (ds : (int*int*int) list, m : int) =
    if null ds
    then 0
    else
	let val count = if #2 (hd ds) = m then 1 else 0
	in
	    count + number_in_month (tl ds, m)
	end;

fun number_in_months (ds : (int*int*int) list, ms : int list) =
    if null ms
    then 0
    else
	number_in_month (ds, hd ms) + number_in_months (ds, tl ms);

fun dates_in_month (ds : (int*int*int) list, m : int) =
    if null ds
    then []
    else
	let val result = dates_in_month (tl ds, m)
	in
	    if #2 (hd ds) = m
	    then hd ds :: result
	    else result
	end;

fun dates_in_months (ds : (int*int*int) list, ms : int list) =
    if null ms
    then []
    else
	dates_in_month (ds, hd ms) @ dates_in_months (ds, tl ms);

fun get_nth (ss : string list, n : int) =
    if n = 1
    then hd ss
    else get_nth (tl ss, n-1);


fun date_to_string (y : int, m : int, d : int) =
    let
	val months = ["January ","February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
    in
	get_nth (months, m)^Int.toString d^", "^Int.toString y
    end



fun number_before_reaching_sum (sum : int, ints : int list) =
    if hd ints >= sum
    then 0
    else
	let val num = number_before_reaching_sum (sum - hd ints, tl ints)
	in
	    if num = 0
	    then hd ints
	    else num
	end;


fun what_month (day : int) =
    let
    val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    fun num_before (sum : int, ds : int list) =
	if hd ds >= sum
	then 1
	else num_before (sum - hd ds, tl ds) + 1
    in
	num_before (day, days)
    end;

fun month_range (d1 : int, d2 : int) =
    let
	val result_length = d2 - (d1 + 1)
	val first_month = what_month (d1)
	val last_month = what_month (d2)
	fun make_range (starting_point, counter) =
	    if counter = 0
	    then []
	    else
		make_range (starting_point, counter - 1) @ [what_month(starting_point + counter)]
    in
	if d1 > d2 orelse result_length < 1
	then [first_month, last_month]
	else
	    first_month :: make_range (d1, result_length) @ [last_month]
    end;

fun oldest (ds : (int*int*int) list) =
    if null ds
    then NONE
    else
	let val oldest_date = oldest(tl ds)
	in
	    if isSome oldest_date
	    then if is_older(valOf oldest_date, hd ds) then oldest_date else SOME (hd ds)
	    else SOME (hd ds)
	end;

