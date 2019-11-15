module fold_example
open FsCheck
open System.Collections.Generic
open System

let add a b = a + b
let foldSum = List.fold add 0

let avg (oldSum, oldCount) (number) = oldSum + number, oldCount + 1

let foldAvg list=
    let sum, count = List.fold avg (0.0,0) list
    float sum / float count

let main = 
    printfn "%i" (foldSum [1;5;4])
    Check.QuickThrowOnFailure (fun list -> foldSum list = List.sum list)
    0
