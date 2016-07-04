module NFA2DFA

open NFA
open RegexModel

// DFA model for NFA to DFA conversion
type DFAState = Set<State>
type DFATransition = DFAState * char * DFAState
type DFATransitionTable = list<DFATransition>

// returns states transitively reachable from any of state T on symbol None
let rec emptyClosureRec (sourceStates:Set<State>) (out:Set<State>) (n:NFA) : Set<State> =
    if sourceStates.IsEmpty then
        out
    else
        let reachableStates = sourceStates |> Set.map (fun (s:State) -> getReachableStates s None n) |> Set.unionMany
        let nextSourceStates = Set.difference reachableStates out
        let nextOut = Set.union out reachableStates
        emptyClosureRec nextSourceStates nextOut n

let emptyClosure (sourceStates:Set<State>) (n:NFA) : Set<State> =
    emptyClosureRec sourceStates sourceStates n

// returns states directly reachable from any of state T on symbol a
let move (sourceStates:Set<State>) (a:char) (n:NFA) : Set<State> =
    Set.map (fun (s:State) -> getReachableStates s (Some a) n) sourceStates
    |> Set.unionMany

let rec inspectRegexAlphabet (regex:Regex) : Set<char> =
    match regex with
    | RegexChar(c) -> Set.singleton c
    | RegexStar(subRegex) -> (inspectRegexAlphabet subRegex)
    | RegexConcat(leftRegex, rightRegex) -> Set.union (inspectRegexAlphabet leftRegex) (inspectRegexAlphabet rightRegex)
    | RegexChoice(leftRegex, rightRegex) -> Set.union (inspectRegexAlphabet leftRegex) (inspectRegexAlphabet rightRegex)

let rec convertNFA2DFARec (workStates:Set<DFAState>) (markedStates:Set<DFAState>) (alphabet:Set<char>) (out:DFATransitionTable) (n:NFA) : DFATransitionTable =
    if workStates.IsEmpty then
        out
    else
        let workState = Seq.head workStates
        let nextMarkedStates = markedStates.Add(workState)
        let transitions:list<char * DFAState> = [ for a in alphabet -> (a, emptyClosure (move workState a n) n) ]
        let reachableStates = List.map snd transitions |> Set.ofList
        let nextWorkStates = Set.union (workStates.Remove(workState)) (Set.difference reachableStates markedStates)
        let contains x = List.exists ((=) x)
        let newTransitions = (List.map (fun (a, u) -> (workState, a, u)) transitions)
                             |> List.filter (fun t -> not (contains t out))
        let nextOut = out @ newTransitions
        convertNFA2DFARec nextWorkStates nextMarkedStates alphabet nextOut n

let collectUniqueDFAStates (table:DFATransitionTable) : array<DFAState> =
    let contains x = List.exists ((=) x)
    seq { 
        for (t, a, u) in table do
            yield t
            yield u
    }
    |> Seq.fold (fun uniqueStates s -> if (contains s uniqueStates) then uniqueStates else (uniqueStates @ [s])) []
    |> List.toArray

let findFinalStates (dfaStates:array<DFAState>) (n:NFA) : Set<State> =
    let finalNFAState = getFinalState n
    seq { 
        for stateNum in 0 .. (dfaStates.Length - 1) do
            let dfaState = dfaStates.[stateNum]
            if dfaState.Contains(finalNFAState) then
                yield stateNum
    }
    |> Set.ofSeq

let transformTransitionTableToDFATransitionFromState (stateNumByStateMap:Map<DFAState, State>) (table:DFATransitionTable) (sourceState:DFAState) : Map<char, State> =
    seq {
        for (t, a, u) in table do
            if t = sourceState then
                yield (a, stateNumByStateMap.[u])
    }
    |> Map.ofSeq

let transformTransitionTableToDFATransitions (dfaStates:array<DFAState>) (table:DFATransitionTable) : array<Map<char, State>> =
    let stateNumByStateMap = dfaStates |> Array.mapi (fun stateNum dfaState -> (dfaState, stateNum)) |> Map.ofArray
    dfaStates |> Array.map (transformTransitionTableToDFATransitionFromState stateNumByStateMap table)

let convertNFA2DFA (regex:RegexModel.Regex) (n:NFA) : DFA =
    let initialState = emptyClosure (set [0]) n
    let initialStates = set [initialState]
    let alphabet = inspectRegexAlphabet regex
    let table = convertNFA2DFARec initialStates Set.empty alphabet List.empty n

    let dfaStates = collectUniqueDFAStates table

    // make sure that the "initialState" is first in the list
    let initialStateIndex = Array.findIndex ((=) initialState) dfaStates
    let firstState = dfaStates.[0]
    dfaStates.[0] <- dfaStates.[initialStateIndex]
    dfaStates.[initialStateIndex] <- firstState

    {
        DFA.transitions = transformTransitionTableToDFATransitions dfaStates table;
        DFA.finalStates = findFinalStates dfaStates n;
    }
