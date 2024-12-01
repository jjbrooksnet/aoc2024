open System

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs

open System.Text.RegularExpressions

module Day1 = 
    module Common =
        let lineToNumbers line =
            let parser = Regex "^([0-9]+)[ \t]+([0-9]+)$"
            let result = parser.Match line
            (result.Groups.[1].Value |> int, result.Groups.[2].Value |> int)
        let loadData () =
            System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt"
            |> Array.map lineToNumbers
            |> Array.unzip
            ||> fun a b -> (Array.sort a, Array.sort b)
    module Simple =
        let similarity xs n = n * (Array.where (fun x -> x = n) xs |> Array.length)
        let calc list1 list2 =
            list1 |> Array.map (similarity list2) |> Array.sum
    module Funky =
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
        let calc list1 list2 =
            similarityCalculator (list1 |> Array.toList) (list2 |> Array.toList) 0 0 0

[<MemoryDiagnoser>]
type Benchmarks() =
    let (list1, list2) = Day1.Common.loadData ()
    [<Benchmark(Baseline = true)>]
    member this.simple () = Day1.Simple.calc list1 list2
    [<Benchmark>]
    member this.funky () = Day1.Funky.calc list1 list2

BenchmarkRunner.Run<Benchmarks>() |> ignore