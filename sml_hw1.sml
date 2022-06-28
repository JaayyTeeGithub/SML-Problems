(* SML comments appear like this *)
(* Jeremy Thomas *)


(* #1 - pow *)
	(* base case is if exponent < 1 then return 1 *)
fun pow (a, b) = if b < 1 then 1
	(* if exponent is > 1 multiply number by pow with exponent decreased by 1 *) 
else a * pow(a, b - 1);


(* #2 - sumTo *)
	(* base case is when x = 0 *)
fun sumTo x = if x = 0 then real(x)
	(* if x not 0 then add the reciprocal of x to sumTo with x decreased by 1 *) 
else 1.0 / real(x) + sumTo(x - 1);
               
               
(* #3 - repeat *)
	(* base case is when num of repeats < 1 *)
fun repeat (s, n) = if n < 1 then ""
	(* if number of repeat > 1 concat string with repeat function with repeats decreased by 1 *)
else s ^ repeat(s, n - 1);


(* #4 - binary *)
	(* base case is when x <= 1 return string of x *)
fun binary x = if x <= 1 then "0" ^ Int.toString(x)
	(* if x <= 1 return binary digit string and feed div value back to function recursively *)
else (binary(x div 2)) ^ Int.toString(x mod 2);


(* #5 - countNegative *)
	(* base case is if no values remain in list then return 0 *)
fun countNegative x = if null x then 0
	(* if list hea1d is < 0 it is negative add 1 and function given tail of list *)
else if (hd x) < 0 then 1 + countNegative(tl x)
	(* if not negative then add zero and function given tail of list *)
else 0 + countNegative(tl x);


(* #6 - absList *)
	(* helper function that returns #1 in tuple *)
fun getOne(tuple: int * int) = #1 tuple;
	(* helper function that returns #2 in tuple *)
fun getTwo(tuple: int * int) = #2 tuple;
	(* base case is if the tail of list = null, if so return list with abs of last tuple *)
fun absList x = if null (tl x) = true then [(abs(getOne(hd x)), abs(getTwo(hd x)))]
	(* if a tail does exist for list cons head of list with the function given the tail as a parameter *) 
else (abs(getOne(hd x)), abs(getTwo(hd x))) :: absList(tl x);



(* #7 - split *)
	(* base case is if list is empty *)
fun split x = if null x then nil
	(* if list not empty but head is 0 make 0 * 0 tuple and @ with function given tail *)
else if hd x = 0 then [(0, 0)] @ split(tl x)
	(* if list not empty but head is even make head div 2 * head div 2 tuple and @ with recursive function *)
else if hd x mod 2 = 0 then [(hd x div 2, hd x div 2)] @ split(tl x)
	(* if list not empty and head is odd return head div 2 * head div 2 + 1 tuple and @ with recursive function *)
else if hd x mod 2 = 1 then [(hd x div 2, hd x div 2 + 1)] @ split(tl x)
	(* do nothing *)
else nil;



(* #8 - isSorted *)
	(* if x = [] true *)
fun isSorted x = if x = [] then true
	(* base case is if tail is null *)
else if null (tl x) then true
	(* if hd x <= hd(tlx) keep looking *)
else if hd x <= hd(tl x) then isSorted(tl x)
else false;

 
(* #9 - collapse *)
	(* base case returns nil if list is empty *) 
fun collapse x = if null x then nil
	(* if tail is not empty return list of head + head of tail and @ with recursive function given the tail of tail *)
else if (tl x) <> nil then [((hd x) + hd(tl x))] @ collapse(tl(tl x))
	(* if tail is empty the return list with head and @ with recursive function given tail *)
else if null (tl x) then [hd x] @ collapse (tl x)
	(* do nothing *)
else nil;

        
(* #10 - insert *)
	(* so i kinda have two base cases *)
	(* one for when the value is the largest in list *)
	(* one for when it finds a larger value *)
	(* base case 1 *)        
fun insert(n,x) = if hd x >= n then [n] @ x
	(* base case 2 *)
else if hd x <= n andalso null (tl x) then [hd x] @ [n]
	(* just keep going until one of them happens *)
else [hd x] @ insert(n, (tl x));
