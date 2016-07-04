module Program

open System
open RegexAPI
    
let matchAndPrint pattern text expected =
    let matchResult = matchString pattern text
    let success = if matchResult = expected then "PASS" else "FAIL"
    let showOperator = if expected then "=~" else "!~"
    printfn "%s : %A %s %A" success text showOperator pattern

let runTests () : unit =
    matchAndPrint "(a|b)*abb" "abb" true
    matchAndPrint "(a|b)*abb" "babb" true
    matchAndPrint "(a|b)*abb" "aabb" true
    matchAndPrint "(a|b)*abb" "aaaabb" true
    matchAndPrint "(a|b)*abb" "bbbabb" true
    matchAndPrint "(a|b)*abb" "aababababb" true
    matchAndPrint "(a|b)*abb" "babaabaabb" true
    matchAndPrint "(a|b)*abb" "" false
    matchAndPrint "(a|b)*abb" "x" false
    matchAndPrint "(a|b)*abb" "abbc" false
    matchAndPrint "(a|b)*abb" "xabb" false
    matchAndPrint "(a|b)*abb" "ab" false
    matchAndPrint "(a|b)*abb" "aaaab" false
    matchAndPrint "(a|b)*abb" "abababaaab" false

[<EntryPoint>]
let main (argv:string[]) =
    if Array.isEmpty argv then
        runTests ()
    else
        let funcName = argv.[0]
        let pattern = argv.[1]
        printfn "%s" (generateMatcherCPP funcName pattern)
    0 // return an integer exit code
