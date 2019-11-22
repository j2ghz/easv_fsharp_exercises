module WordGuesser
open System

type wordsJson = FSharp.Data.JsonProvider<"https://raw.githubusercontent.com/bevacqua/correcthorse/master/wordlist.json">

let words = wordsJson.GetSample().Strings |> Array.toList
//let words = ["test"]

type State = {
    word: string
    guessesdChars: char list
    guesses: int
    won: bool
}

let round (input:string) state =
    match input.ToCharArray() with
    | [|c|] when List.contains c state.guessesdChars -> 
        printfn "You already tried that!"
        {state with guesses = state.guesses - 1}
    | [|c|] -> 
        {state with guessesdChars    = c :: state.guessesdChars   }
    | guess when input = state.word -> 
        printfn "You win!"
        {state with won = true}
    | guess when input <> state.word -> 
        printfn "Wrong guess!"
        state
    | _ -> 
        printfn "Invalid guess"
        state
let random = Random()
let getRandom list = random.Next(List.length list) |> List.item <| list

let winner state =
    String.forall (fun c -> List.contains c state.guessesdChars) state.word

let printGameState state =
    let stars = state.word |> String.map (fun c -> if state.guessesdChars |> List.contains c then c else '*') 
    printf "%s   Used %A. Guess: " stars state.guessesdChars

let play() =
    printfn "Welcome to WordGuesser"
    let mutable state = {word = getRandom words; guessesdChars = []; guesses = 0; won=false; }
    while not state.won do
        printGameState state
        let input = System.Console.ReadLine()
        state <- round input state
        state <- {state with guesses = state.guesses + 1; won = state.won || winner state}
    printfn "You won using only %i guessesdChars!" state.guesses
        