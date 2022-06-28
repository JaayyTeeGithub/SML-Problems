(* SML comments appear like this *)
(* Jeremy Thomas *)

(* #1 - quicksort *)
(* if partition is given a pivot and no other values return nil for above and below *)
fun partition (pivot, nil) = (nil, nil) 
(* if partition is given pivot and a list, separate list into first value and rest *)
| partition (pivot, first::rest) = 
(* set val of above and below in let expr to above and below returned by function  *)
	let 
		val (below, above) = partition(pivot, rest)
	in
	(* if pivot is less than first add first to below *)
		if first < pivot then (first::below, above)
	(* else add first to above *)
		else (below, first::above)
	end;	
(* return nil if quicksort is given nil *)
fun quicksort nil = nil
(* if quicksort is given list of len 1 return list *)
| quicksort [a] = [a]
(* if quicksort is given a list separate list into first and rest *)
| quicksort (first::rest) =
	let
	(* set below and above in let to return of partition *)
		val (below, above) = partition(first, rest)
	in 
	(*	
	quicksort above and below
	below is attached to left of pivot and above to the right
	*)
	
		quicksort(below) @ [first] @ quicksort(above)
	end;

(* #2 - member *)
(* if member is given an empty list return false *)
fun member (e, []) = false
(* if member is given a list separate list into first and rest *)
| member (e, first::rest) =
(* if element is also first return true *)
	if e = first then true
	(* else recursively call member with rest of list *)
	else member(e, rest);
               
               
(* #3 - returns the union of sets (lists) s1 and s2*)
(* You may assume that s1 and s2 do not have any duplicate elements *)
(* is union if given empty s2 return s1 *)
fun union (s1, []) = s1
(* if union is given s1 and s2 separate s2 into first and rest *) 
| union (s1, s2first::s2rest) =
(* if first in s2 is a member of s1 recursively call union with s1 and rest of s2 *)
	if  member(s2first, s1) then union(s1, s2rest)
	(* if first of s2 is not a member of s1 cons first of s2 with recursive call of union with s1 and the rest of s2 *)
	else s2first::union(s1, s2rest);


(* #4 - returns the intersection of sets (lists) s1 and s2 *)
(* You may assume that s1 and s2 do not have any duplicate elements *)
(* if intersection recieves empty s2 return empty list *)
fun intersection (s1, []) = []
(* if intersection recieves s1 and s2 then separate s2 into first and rest *)
| intersection (s1, s2first::s2rest) =
(* if first of s2 is a member of s1 cons first of s2 with recurive call of intersection with s1 and the rest of s2 *)
	if member(s2first, s1) then s2first::intersection(s1, s2rest)
	(* if first of s2 is not a member of s1 recursively call intersection with s1 and the rest of s2 *)
	else intersection(s1, s2rest);

(* #5 - Return list of integers from start (inclusive) to stop (exclusive) by step *)
fun range (start, stop, step) = 
(* check to see if step is negative *)
	if step < 0 then 
	(* if start is greater than stop append start to recurive call of range with start increased by step *)
		if (start > stop) then [start] @ range((start + step), stop, step)
		(* if start is less than stop return nil *)
		else nil	
(* if step is positive *)
	else 
	(* if start is less than stop append start to recursive call of range with start increase by step *)
		if (start < stop) then [start] @ range((start + step), stop, step)
		(* if start is greater than stop return nil *)
		else nil;

(* #6 - Return a slice of a list between indices start inclusive, and stop exclusive. Assume first element of list is at index 0*)
(* if slice is given empty list return empty list *)
fun slice([], start, stop) = []
(* is slice is given a list separate into first and rest *)
| slice (first::rest, start, stop) =
(* if start is less than or equal to 0 and stop is greater than or equal to 1 append first to recurive call of slice decrementing stop and start by 1 *)
	if (start <= 0) andalso (stop >= 1) then [first] @ slice(rest, start - 1, stop - 1)
	(* if start and stop do not fall within appropriate range recursively call slice and decrement stop and start by 1 *)
	else slice(rest, start - 1, stop - 1);