let gcd  (x: int)(y: int) : int =
 let min = if x < y then x else y
in 
 let rec decrement (d:int) : int =
   if x mod  d =0 && y mod d =0
    then d
  else decrement(x-1)
in decrement min


let lcm (x:int) (y:int) :int=
 let max = if x > y then x else y 
  in 
  let rec increment i =
    if x mod i =0 && y mod i = 0
    then i
    else  increment(i+1)
in increment max

let sum_a (n:int) : int =
  let rec sum sum_so_far i =
    if i =n
      then sum_so_far+i
      else sum(sum_so_far+i) (i+1)
    in sum 0 0 

  let rec sum x=  
    match x with 
    | [] ->0

  let rec sum xs =
    match xs with
    | [] -> 0
    | x::rest -> x + sum rest

let rec all (bs: bool list) : bool =
   match bs with 
   | [] -> true
   | h::t ->  h && all t
  let rec inc_all(nums : int list) : int list =
    match nums with 
    | [] -> []
    | h::t ->(h+1) :: inc_all t

    let rec evens (nums: int list) : int list =
      match nums with 
      | [] -> []
      | h::t -> 
        if h mod 2 = 0 
         then h :: evens t 
        else evens t
