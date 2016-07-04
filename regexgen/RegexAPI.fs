module RegexAPI

open RegexModel

type PatternMatcher = string -> bool

let compilePatternToDFA (pattern:string) : DFA =
    let regex = RegexStringParser.parseRegexString pattern
    let nfa = NFA.buildNFA regex
    NFA2DFA.convertNFA2DFA regex nfa

let compilePattern (pattern:string) : PatternMatcher =
    let dfa = compilePatternToDFA pattern
    DFAMatcher.matchStringWithDFA dfa    

let matchString (pattern:string) (text:string) : bool =
    compilePattern pattern text

let generateMatcherCPP (funcName:string) (pattern:string) : string =
    let dfa = compilePatternToDFA pattern
    GeneratorCPP.generateMatcherCPP funcName dfa
