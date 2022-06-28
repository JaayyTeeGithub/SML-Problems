(* SML comments appear like this *)
(* Jeremy Thomas *)

(* #1 - duplist *)
(* anonymous function takes a and b and cons two as to the next item to be folded, adds these to list *)
fun duplist x = foldr(fn (a,b) => a::a::b) [] x;

(* #2 - mylength *)
(* the anonymous function adds one for each item folded *)
fun mylength x = foldr(fn (a,b) => 1 + b) 0 x;

(* #3 - il2absrl *)
(* anonymous function adds the real of the abs of a to the next to be folded, adds to list *)
fun il2absrl x = foldr(fn (a,b) => real(abs(a))::b) [] x;

(* #4 - myimplode *)
(* foldr string concats strings return by a foldr *)
(* the second foldr has an anonymous function that turns a into a string and adds it to a list *)
fun myimplode x = foldr (op ^) "" (foldr (fn (a,b) => String.str(a)::b) [] x);

(* #5 - lconcat *)
(* goes through lists in list and concats them *)
fun lconcat x = foldr (fn (a, b) => a @ b) [] x;

(* #6 - convert *)
(* takes a list of tuples, maps over each tuple and assigns a to a list and b to a list *)
fun convert x = (map (fn (a,b) => a) x, map (fn (a,b) => b) x);

(* #7 - mymap *)
(* mymap is given a function and a list *)
(* foldr runs on the list adding f(a) to b in a list *)
fun mymap f x = foldr(fn (a,b) => f(a)::b) [] x;

(* #8 - myfoldl *)
(* if myfoldl is given empty list return initialValue *)
fun myfoldl f initialValue [] = initialValue
(* if myfoldl is given a list run with given function and execute on the rest *)
|	myfoldl f initialValue (x::xs) = myfoldl f (f (x, initialValue)) xs;