module RegexModel

// model
type Regex =
    | RegexChar of char // a
    | RegexStar of Regex // s*
    | RegexConcat of (Regex * Regex) // st
    | RegexChoice of (Regex * Regex) // s|t

// DFA model for processing (matching)
type State = int
type DFA = {
    transitions: array<Map<char, State>>;
    finalStates: Set<State>;
}
