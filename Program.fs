// Learn more about F# at http://fsharp.org

open System

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

let converter i = sprintf "%i" i

let a = 
    [1;2] 
    |> List.map converter

[<EntryPoint>]
let main argv =
    printfn "%i" (e1_4 4)
    printfn "%i" (e1_5 4)
    printfn "%i" (e2_9 (2,3))
    0 // return an integer exit code
