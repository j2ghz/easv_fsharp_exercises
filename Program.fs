// Learn more about F# at http://fsharp.org

open System

module Week44 =
    let e11 i = i+4
    let e12 (x, y) = System.Math.Sqrt((x * x)  + (y * y))

    // Exercise 1.3
    let e13_a = (fun g -> g + 4)
    let e13_b = fun (x,y) -> System.Math.Sqrt((x * x)  + (y * y))

    let rec e1_4 = function
    | 0 -> 0
    | n -> (e1_4 (n-1)) + n

    let rec e1_5 = function
    | 0 -> 0
    | 1 -> 1
    | n -> e1_5 (n-1) + e1_5 (n-2)

    let rec e1_6 = function
    | m,0 -> m
    | m,n -> m+n + e1_6 (m,n-1)


    let e2_2 (s,n) = 
        (fun _ -> s)  
        |> Seq.init n 
        |> String.concat ""

    let e2_5 (s:string) (c:char) =
        s.ToCharArray()
        |> Array.where (fun sc -> sc = c)
        |> Array.length

    let rec e2_9 = function
    | (0,y) -> y
    | (x,y) -> e2_9 (x-1,x*y)
    // int * int -> int
    // x < 0
    // f(2,3) = f(1,6) = f(0,6) = 6
    // function f with two parameters f and y

    let e2_12 (f: int -> int) : int = 
        Seq.initInfinite id
        |> Seq.map (fun i -> i,f i)
        |> Seq.skipWhile (snd >> (<>) 0)
        |> Seq.item 0
        |> fst

//Week45
module Week45 =

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
        | [] -> [x]

    //4.11.3
    let rec intersect = function
    | x::xs,y::ys when x = y -> x :: intersect (xs,ys)
    | x::xs,y::ys when x > y -> intersect (x::xs,ys)
    | x::xs,y::ys when x < y -> intersect (xs,y::ys)
    | [],_
    | _,[] -> []

    //4.11.4
    let rec union = function
    | x::xs,y::ys when x = y -> x :: y :: union (xs,ys)
    | x::xs,y::ys when x > y -> y:: union (x::xs,ys)
    | x::xs,y::ys when x < y -> x::union (xs,y::ys)
    | [],rest
    | rest,[] -> rest

    //4.11.5
    let rec minus = function
    | x::xs,y::ys when x = y -> minus (xs,ys)
    | x::xs,y::ys when x > y -> minus (x::xs,ys)
    | x::xs,y::ys when x < y -> x::minus (xs,y::ys)
    | rest,[] -> rest
    | [],_ -> []

    //4.12
    let rec sum p list =
        match list with
        | x::xs ->
            if p x then
                x + sum p xs
            else 
                sum p xs
        | [] -> 0
    //b
    //Construct a function that takes a list of integers as input and return a reduced list of pairs representing the input. For example; input [1;3;2;2;1] will give [(1,2);(3,1);(2,2)] as output. The pairs must contain the value in the first component and the counter in the second.
    let reduce (xs:int list) = List.countBy id xs

    //c
    //If k neighbour values of an array are increasing, we say that the array contains a monotone segment of length k. For instance, the array (*) [3, 2, 1, 3, 5, 7, 9, 2, 4] contains [1, 3, 5, 7, 9] and [2, 4] as some monotone segments. Construct a function in F# computing the length of a maximal monotone segment of an array. The function should return 5 with (*) as input.
    let maxMonotone list = list |> List.fold (fun (max,items) item -> if (List.isEmpty items) || (item > List.head items) then (max,item::items) else (Math.Max(max,List.length items), []) ) (0,[]) |> fst

    //d
    //Construct a function with two parameters; a string s and a boolean ignoreSpace. The function should return true if s is a palindrome, where spaces must be ignored if and only if ignoreSpace is true.
    let palindrome ignoreSpace (s:string)  =
        let arr = s.ToCharArray()
        let arr' = if ignoreSpace then arr |> Array.where (Char.IsWhiteSpace >> not) else arr
        Array.rev arr' = arr'
    
open Week44
open Week45
open FsCheck
open System.Globalization

let reversed s=
    seq {
            let tee = StringInfo.GetTextElementEnumerator(s)
            while tee.MoveNext() do 
                yield tee.GetTextElement() 
        } |> Array.ofSeq           |> Array.rev |> String.concat ""

let test() =
    printfn "%i" (e1_4 4)
    printfn "%i" (e1_5 4)
    printfn "%i" (e2_9 (2,3))
    Check.QuickThrowOnFailure (fun ls i -> (i::ls |> List.sort) = (ls |> List.sort |> Week45.insert i) )
    assert (intersect ([1;1;1;2;2], [1;1;2;4]) = [1;1;2])
    Check.QuickThrowOnFailure (fun (xs:int list) ys ->
        let xss = List.sort xs
        let yss = List.sort ys
        (union (xss,yss)) = (List.append xss yss |> List.sort)
        )
    assert(minus([1;1;1;2;2],[1;1;2;3]) = [1;2])
    assert(minus([1;1;2;3],[1;1;1;2;2]) = [3])
    Check.QuickThrowOnFailure (fun p xs -> sum p xs = (xs |> List.where p |> List.sum) )
    assert(maxMonotone [3; 2; 1; 3; 5; 7; 9; 2; 4] = 5)
    assert(palindrome true "rad ar" )
    fold_example.main

[<EntryPoint>]
let main argv = 
    while true do
        WordGuesser.play()
    0