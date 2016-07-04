module DFAMatcher

open RegexModel

let rec matchStringWithDFAFromState (dfa:DFA) (text:list<char>) (currentState:State) : bool =    
    match text with
    | c :: tail ->
        let transitions = dfa.transitions.[currentState]
        if transitions.ContainsKey(c) then
            let nextState = transitions.[c]
            matchStringWithDFAFromState dfa tail nextState
        else
            false
    | _ ->
        dfa.finalStates.Contains(currentState)

let matchStringWithDFA (dfa:DFA) (text:string) : bool =    
    matchStringWithDFAFromState dfa (Seq.toList text) 0
