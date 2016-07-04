module RegexStringParser

open System
open RegexModel

// grammar
type GRegexChoice =
    | GRegexChoiceComposite of (GRegexConcat * GRegexChoice)  // s|t
    | GRegexJustConcat of GRegexConcat
and GRegexConcat =
    | GRegexConcatComposite of (GRegexStar * GRegexConcat) // st
    | GRegexJustStar of GRegexStar
and GRegexStar =
    | GRegexStarComposite of GRegexGroup // s*
    | GRegexJustGroup of GRegexGroup
and GRegexGroup =
    | GRegexGroupComposite of GRegexChoice // (s)
    | GRegexChar of char // a

// parsers
type InputList = list<char>
type ParserResult<'rule> = 'rule * InputList

exception ParseError of string

let isAlphabetChar (c:char) : bool = Char.IsLetter(c)
let isStartChar (c:char) : bool = (isAlphabetChar c) || (c = '(')

let matchAndSkipChar (c:char) (input:InputList) : InputList =
    match input with
    | c :: rest -> rest
    | _ -> raise (ParseError("matchChar"))

let rec parseGRegexChoice (input:InputList) : ParserResult<GRegexChoice> =
    let (leftExprRule, leftExprTail) = parseGRegexConcat input
    match leftExprTail with
    | '|' :: rest ->
        let (rightExprRule, rightExprTail) = parseGRegexChoice rest
        (GRegexChoiceComposite(leftExprRule, rightExprRule), rightExprTail)
    | _ -> (GRegexJustConcat(leftExprRule), leftExprTail)

and parseGRegexConcat (input:InputList) : ParserResult<GRegexConcat> =
    let (leftExprRule, leftExprTail) = parseGRegexStar input
    match leftExprTail with
    | x :: rest when isStartChar(x) ->
        let (rightExprRule, rightExprTail) = parseGRegexConcat leftExprTail
        (GRegexConcatComposite(leftExprRule, rightExprRule), rightExprTail)
    | _ ->
        (GRegexJustStar(leftExprRule), leftExprTail)

and parseGRegexStar (input:InputList) : ParserResult<GRegexStar> =
    let (subExprRule, subExprTail) = parseGRegexGroup input
    match subExprTail with
    | '*' :: resultTail -> (GRegexStarComposite(subExprRule), resultTail)
    | _ -> (GRegexJustGroup(subExprRule), subExprTail)

and parseGRegexGroup (input:InputList) : ParserResult<GRegexGroup> =
    match input with
    | '(' :: rest ->
        let (subExprRule, subExprTail) = parseGRegexChoice rest
        let resultTail = matchAndSkipChar ')' subExprTail
        (GRegexGroupComposite(subExprRule), resultTail)
    | a :: rest when isAlphabetChar(a) -> (GRegexChar(a), rest)
    | _ -> raise (ParseError("GRegexGroup"))

let parseRegexChars (regex:InputList) : GRegexChoice =
    let (rule, tail) = parseGRegexChoice regex
    if tail.Length = 0 then
        rule
    else
        raise (ParseError("parseRegexChars"))
    
// converters
let rec convertGRegexChoiceToRegex (rule:GRegexChoice) : Regex =
    match rule with
    | GRegexChoiceComposite(concat, choice) -> RegexChoice(convertGRegexConcatToRegex concat, convertGRegexChoiceToRegex choice)
    | GRegexJustConcat(concat) -> convertGRegexConcatToRegex concat
and convertGRegexConcatToRegex (rule:GRegexConcat) : Regex =
    match rule with
    | GRegexConcatComposite(star, concat) -> RegexConcat(convertGRegexStarToRegex star, convertGRegexConcatToRegex concat)
    | GRegexJustStar(star) -> convertGRegexStarToRegex star
and convertGRegexStarToRegex (rule:GRegexStar) : Regex =
    match rule with
    | GRegexStarComposite(group) -> RegexStar(convertGRegexGroupToRegex group)
    | GRegexJustGroup(group) -> convertGRegexGroupToRegex group
and convertGRegexGroupToRegex (rule:GRegexGroup) : Regex =
    match rule with
    | GRegexGroupComposite(choice) -> convertGRegexChoiceToRegex choice
    | GRegexChar(c) -> RegexChar(c)


// public function
let parseRegexString : string -> Regex =
    Seq.toList >> parseRegexChars >> convertGRegexChoiceToRegex
