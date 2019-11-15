module fold_example

let add a b = a + b
let foldSum = List.fold add 0

let avg (oldSum, oldCount) number = oldSum + number, oldCount + 1

let foldAvg list =
    let sum, count = List.fold avg (0,0) list
    sum / count

let main = 
    printfn "%i" (foldSum [1;5;4])
    printfn "%i" (foldAvg [1;5;4])
    0
