
(*Sum of the First n Naturals*)
fun sum 0 = 0
  | sum n = n + sum (n-1)

(*Sum of the First n Squares*)
fun sumsq 0  = 0
  | sumsq n = n*n + sumsq (n-1);

(*Sum of the First n Odd Naturals*)
fun sumOdd 0  = 0
  | sumOdd n = 2*n+1 + sumOdd (n-1);

(*fib*)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2);

(*Sum of elements in the list*)
fun sumList [] = 0
 | sumList (a::b) = a + sumList (b)

(*zip*)
fun zip [] [] = nil
 | zip a [] = nil
 | zip [] b = nil
 | zip (a::b) (c::d) = [a,c]::(zip b d)

fun leq a b = if ( a < b) then true else false

 (*partition*)
 fun partitionlist pivot nil f = ([], [])
  | partitionlist pivot (first::remaining) f =
    let
      val (left, right) = partitionlist pivot remaining f
    in
      if (f first pivot)
      then (first::left, right)
      else (left, first::right)
    end

(*Quick Sort*)
fun qsort nil f = []
 | qsort (first::remaining) f =
  let
    val (left, right) = partitionlist first remaining f
  in
      (qsort left f) @ [first] @ (qsort right f)
  end

(*map*)
 fun map [] f = []
  | map (a::bs) f = f(a)::(map bs f)


(*reverse*)
fun reverse nil = []
  | reverse (a::b) = reverse b @ [a]

(*find/filter*)
fun filter pred [] = []
  | filter pred (a::b) =
    if (pred a)
    then a::filter pred b
    else filter pred b
