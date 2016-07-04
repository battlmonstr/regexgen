module NFA

open RegexModel

type StateTransitions = Map<Option<char>, Set<State>>
type NFA = array<StateTransitions>

let getInitialState (n:NFA) : State = 0
let getFinalState (n:NFA) : State = n.Length - 1
let getFinalStateTransitions (n:NFA) : StateTransitions = n.[getFinalState n]
let getReachableStates (source:State) (transitionLabel:Option<char>) (n:NFA) : Set<State> =
    let transitions:StateTransitions = n.[source]
    if transitions.ContainsKey(transitionLabel) then transitions.[transitionLabel] else Set.empty

let buildNFARegexChar (a:char) : NFA =
    let finalState = 1
    let initialStateTransitions = Map.ofList [(Some(a), set [finalState])]
    let finalStateTransitions = Map.empty
    [| initialStateTransitions; finalStateTransitions |]

let addOffsetToStates (off:int) (states:Set<State>) : Set<State> =
    Set.map (fun state -> state + off) states
let addOffsetToStatesOfStateTransitions (off:int) (transitions:StateTransitions) : StateTransitions =
    Map.map (fun c states -> addOffsetToStates off states) transitions
let addOffsetToStatesOfNFA (off:int) (n:NFA) : NFA =
    Array.map (addOffsetToStatesOfStateTransitions off) n 

let buildNFARegexConcat (nleft:NFA) (nright:NFA) : NFA =
    let connState = getFinalState nleft
    let adjustedLeftNFA = Array.sub nleft 0 connState
    let adjustedRightNFA = addOffsetToStatesOfNFA connState nright
    Array.append adjustedLeftNFA adjustedRightNFA

let addTransitionFromFinalState (transition:Option<char> * Set<State>) (n:NFA) : NFA =
    let frontSlice = (Array.sub n 0 (n.Length - 1))
    let finalStateTransitions = Map.ofList [transition]
    Array.append frontSlice [| finalStateTransitions |]

let buildNFARegexChoice (nleft:NFA) (nright:NFA) : NFA =
    let connState = nleft.Length + 1
    let finalState = connState + nright.Length
    let addTransitionToFinalState = addTransitionFromFinalState (None, set [finalState])
    let adjustedLeftNFA = addOffsetToStatesOfNFA 1 nleft |> addTransitionToFinalState
    let adjustedRightNFA = addOffsetToStatesOfNFA connState nright |> addTransitionToFinalState
    let initialStateTransitions = Map.ofList [(None, set [1; connState])]
    let finalStateTransitions = Map.empty
    Array.concat [
        [| initialStateTransitions |];
        adjustedLeftNFA;
        adjustedRightNFA;
        [| finalStateTransitions |]
    ]

let buildNFARegexStar (n:NFA) : NFA =
    let finalState = n.Length + 1
    let addTransitionsToFinalState = addTransitionFromFinalState (None, set [1; finalState])
    let adjustedNFA = addOffsetToStatesOfNFA 1 n |> addTransitionsToFinalState
    let initialStateTransitions = Map.ofList [(None, set [1; finalState])]
    let finalStateTransitions = Map.empty
    Array.concat [
        [| initialStateTransitions |];
        adjustedNFA;
        [| finalStateTransitions |]
    ]

let rec buildNFA (regex:Regex) : NFA =
    match regex with
    | RegexChar(c) -> buildNFARegexChar c
    | RegexStar(subRegex) -> buildNFARegexStar (buildNFA subRegex)
    | RegexConcat(leftRegex, rightRegex) -> buildNFARegexConcat (buildNFA leftRegex) (buildNFA rightRegex)
    | RegexChoice(leftRegex, rightRegex) -> buildNFARegexChoice (buildNFA leftRegex) (buildNFA rightRegex)
