module GeneratorCPP

open RegexModel
open System

let generateMatcherCPP (funcName:string) (dfa:DFA) : string =    
    seq {
        // header
        yield! [
            "#include \"stdafx.h\"";
            "#include <string>";
            "#include <unordered_map>";
            "#include <unordered_set>";
            "";
        ]
        
        // signature
        yield (Printf.sprintf "bool %s(std::string text) {" funcName)
        
        // DFA.transitions
        yield "\tstatic const std::unordered_map<char, int> transitions[] = {"
        for t in dfa.transitions do
            yield "\t\tstd::unordered_map<char, int>({"
            for KeyValue(c, next) in t do
                yield Printf.sprintf "\t\t\t{ '%c', %d }," c next
            yield "\t\t}),"
        yield "\t};";
        
        // DFA.finalStates
        yield "\tstatic const std::unordered_set<int> finalStates = {"
        for f in dfa.finalStates do
            yield Printf.sprintf "\t\t%d," f
        yield "\t};";

        // algorithm
        yield! [
            "\tint currentState = 0;";
            "\tfor (auto it = text.begin(); it != text.end(); it++) {";
            "\t    auto t = transitions[currentState];";
            "\t    auto nextState = t.find(*it);";
            "\t    if (nextState != t.end()) {";
            "\t        currentState = nextState->second;";
            "\t    } else {";
            "\t        return false;";
            "\t    }";
            "\t}";
            "\treturn (finalStates.find(currentState) != finalStates.end());";
        ]

        // emd function
        yield "}"
    }
    |> String.concat Environment.NewLine
