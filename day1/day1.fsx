//Start with 1 sample input line to test a parsing function we'll need
let line1 = "3  4"

//Parsing the Active Patterns is the F# way, but I'm more familiar with RegExes
open System.Text.RegularExpressions

//Each line is a pair of numbers, so define a function that converts a line to a tuple of ints
//The input is guaranteed to be well-formed, so we don't need to worry about non-matching lines
//Also, the RegEx groups will always be numeric and within int range, so converting string->int is safe
let lineToNumbers line =
    let parser = Regex "^([0-9]+)[ \t]+([0-9]+)$"
    let result = parser.Match line
    (result.Groups.[1].Value |> int, result.Groups.[2].Value |> int)

//Test with our sample input
lineToNumbers line1
//FSI evaluates this to
//val it: int * int = (3, 4)
//So parsing is fine! Let's move on


//For part 2, one way to get to our solution would be to:
//read all lines of the input into an array of strings
//map over the strings with lineToNumbers, giving an array of int * int
//map over the array of tuples with fst, and sort the result
//map over the array of tuples with snd, and sort the result
//That gives us 2 sorted arrays
//zipWith (actually map2) to operate on the 2 arrays simultaneously to get a pair-wise difference
//fold over the differences to get the sum

let lines = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt"
let pairs = lines |> Array.map lineToNumbers
let list1 = pairs |> Array.map fst |> Array.sort
let list2 = pairs |> Array.map snd |> Array.sort
let differences = (list1, list2) ||> Array.map2 (fun a b -> (a - b |> abs))
let total = differences |> Array.sum
//val total: int = 2742123
//We have the solution to part 1. Gold Star!

//For part 2, we will create a similarity function
//It takes a number and an array of numbers as an input and returns the product of the number and the count of that number in the array
//Make the array the first parameter so that we can partially apply the function with list2, and then map the partially-applied function over list1
let similarity xs n = n * (Array.where (fun x -> x = n) xs |> Array.length)
//We can read this as mapping the similarity with list2 over each element in list1
let similarities = list1 |> Array.map (similarity list2)
let score = similarities |> Array.sum
//val score: int = 21328497
//That's the right answer! You are one gold star closer to finding the Chief Historian.

(*
Thoughts on efficiency:
In Part1 it may save space to perform sorted inserts when reading each line, rather than reading the whole input into an array and then also having an intermediate list of pairs

In Part2, we could use the fact that both lists are sorted to traverse each list once

Or rather, both lists once....
Our function (one of a pair) would take
* the number in list1 we're looking at (start from 0),
* everything in list1 we haven't traversed yet,
* everything in list2 we haven't traversed yet,
* and the current total

The complementary function takes
* the number from list1 we're looking for in list2
* and everything in list2 we haven't traversed yet,
and it returns
* the count of elements in list2 that are equal to the number from list1
* and the rest of list2 that is greater that the number from list1
*)

let rec countAndSkip xs n acc =
    match xs with
    | [] -> ([], acc)
    | head :: tail when head < n -> countAndSkip tail n 0
    | head :: tail when head = n -> countAndSkip tail n (acc + 1)
    | _ -> (xs, acc)

let rec similarityCalculator ones twos acc lastN lastNCountInTwos=
    match ones with
    | [] -> acc
    | head :: tail when head = lastN -> similarityCalculator tail twos (acc + (head * lastNCountInTwos)) lastN lastNCountInTwos
    | head :: tail ->
        let (remainingTwos, countN) = countAndSkip twos head 0
        similarityCalculator tail remainingTwos (acc + head * countN) head countN

similarityCalculator (list1 |> Array.toList) (list2 |> Array.toList) 0 0 0
//val it: int = 21328497
//Awesome, we've got the same result!

//Let's benchmark it to see what the difference in the two styles is. Was it worth the head scratching to craft the two tail recursive functions?


//Also, we could possibly have used unzip to get list1 and list2, instead of mapping with fst and snd. Let's try:
let (list1', list2') = Array.unzip pairs ||> fun a b -> (Array.sort a, Array.sort b)
list1 = list1' && list2 = list2'


(*
Benchmark results:
| Method | Mean      | Error    | StdDev   | Ratio | Gen0    | Gen1   | Allocated | Alloc Ratio |
|------- |----------:|---------:|---------:|------:|--------:|-------:|----------:|------------:|
| simple | 238.53 us | 1.812 us | 1.606 us |  1.00 | 21.7285 | 0.2441 | 178.58 KB |        1.00 |
| funky  |  68.90 us | 0.507 us | 0.449 us |  0.29 | 22.8271 | 2.4414 | 187.27 KB |        1.05 |

The funky version is more than 240% faster, so seems to be worthwhile. 10x Gen1 collections is probably due to continually dropping the lists' heads. Alloctions hardly up
*)