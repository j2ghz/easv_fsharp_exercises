module Compulsory1
open FsCheck

//4.11.1
let rec count xs x =
    match xs with
    | i::is when i < x -> count is x
    | i::is when i = x -> 1+count is x
    | i::_ when i > x -> 0
    | [] -> 0
let countAlt xs x =
    xs
    |> List.filter ((=) x)
    |> List.length

//4.11.2
let rec insert (x:int) = function
    | i::is when i < x -> i :: insert x is
    | i::is when i >= x -> x :: i :: is
    | i::is -> failwith "i has always has to be smaller than x, or bigger or equal to x. But without this clause, the compiler complains about incomplete pattern match"
    | [] -> [x]

//4.11.3
let rec intersect = function
| x::xs,y::ys when x = y -> x :: intersect (xs,ys)
| x::xs,y::ys when x > y -> intersect (x::xs,ys)
| x::xs,y::ys when x < y -> intersect (xs,y::ys)
| x::xs,y::ys -> failwith "should never be reached, but compiler complains if omitted"
| [],_
| _,[] -> []

//4.11.4
let rec union = function
| x::xs,y::ys when x = y -> x :: y :: union (xs,ys)
| x::xs,y::ys when x > y -> y:: union (x::xs,ys)
| x::xs,y::ys when x < y -> x::union (xs,y::ys)
| x::xs,y::ys -> failwith "should never be reached, but compiler complains if omitted"
| [],rest
| rest,[] -> rest

//4.11.5
let rec minus = function
| x::xs,y::ys when x = y -> minus (xs,ys)
| x::xs,y::ys when x > y -> minus (x::xs,ys)
| x::xs,y::ys when x < y -> x::minus (xs,y::ys)
| x::xs,y::ys -> failwith "should never be reached, but compiler complains if omitted"
| rest,[] -> rest
| [],_ -> []

let test() =
    //count
    Check.QuickThrowOnFailure (fun (l:int) (list:int list) -> 
        let list = List.sort list
        (count list l) = (countAlt list l))

    //insert
    //Check, that for a random i:integer and is:integer list
    Check.QuickThrowOnFailure (fun i is -> 
        //make a weaklyAscendingList from an arbitrary list
        let weaklyAscendingList = List.sort is
        // a function to compare to. In this case, the result should be the same as if we inserted the item at the start of the list and then sorted it
        let expected = i :: weaklyAscendingList |> List.sort
        // the actual impelementation
        let actual = insert i weaklyAscendingList
        //compare results
        actual=expected
        )

    //intersect
    assert (intersect ([1;1;1;2;2], [1;1;2;4]) = [1;1;2])

    //union
    // union should give same results as appending the lists together and then sorting
    Check.QuickThrowOnFailure (fun (xs:int list) ys ->
        let xss = List.sort xs
        let yss = List.sort ys
        (union (xss,yss)) = (List.append xss yss |> List.sort)
        )
    assert(union([1;1;2],[1;2;4]) = [1;1;1;2;2;4])

    //minus
    assert(minus([1;1;1;2;2],[1;1;2;3]) = [1;2])
    assert(minus([1;1;2;3],[1;1;1;2;2]) = [3])