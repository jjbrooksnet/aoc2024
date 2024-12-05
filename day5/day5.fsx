open System.Text.RegularExpressions
type PageOrderRule = int * int
let pageOrderRegex = Regex "^(\d+)\|(\d+)$"
let stringToPageOrderRule input =
    let parsed = input |> pageOrderRegex.Match
    if parsed.Success then
        Some (parsed.Groups.[1].Value |> int, parsed.Groups.[2].Value |> int )
    else
        None
let (|PageOrderRule|_|) input = stringToPageOrderRule
let orderRulesToMap (rules: PageOrderRule list) =
    rules
    |> List.groupBy fst
    |> List.fold (fun table (prec, succ) -> Map.add prec (succ |> List.map snd) table) Map.empty

let stringToPageList (input: string) =
    input.Split(',') |> Array.map int |> Array.toList

let checkPageListCompliesWith ruleMap pageList =
    let reverseList = List.rev pageList
    let rec checkSublist sublist =
        match sublist with
        | [] -> true
        | [_] -> true
        | page :: precedingPages ->
            if ruleMap |> Map.containsKey page then
                let succedentsFromRules = ruleMap |> Map.find page
                let ruleBreach =
                    succedentsFromRules
                    |> List.filter (fun (succendent) -> precedingPages|> List.contains succendent) 
                    |> List.length > 0
                if ruleBreach then
                    false
                else
                    checkSublist precedingPages
            else
                true
    checkSublist reverseList

let sample = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"



let processInput (input: string) =
    input.Split "\n\n"
    |> (fun (parts) -> (parts.[0].Split '\n', parts.[1].Split '\n'))
    |> (fun (rules, pages) -> (
        rules |> Array.map stringToPageOrderRule |> Array.choose id |> Array.toList |> orderRulesToMap,
        pages |> Array.map stringToPageList
       ))

(*
let (orderMap, pageLists) = processInput sample

pageLists.[0] |> checkPageListCompliesWith orderMap |> string |> printfn "%s" //True
pageLists.[3] |> checkPageListCompliesWith orderMap |> string |> printfn "%s" //False
*)
let getMiddle (from: int list) =
    from.[(from.Length - 1) / 2]

let (orderMap, pageLists) = processInput <| System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\\input.txt"

pageLists
|> Array.filter (checkPageListCompliesWith orderMap)
|> Array.map getMiddle
|> Array.sum
(*
Had an issue with trailing newline in the file. Removed the trailing newline rather than deal with it :>
*)
//val it: int = 5129     