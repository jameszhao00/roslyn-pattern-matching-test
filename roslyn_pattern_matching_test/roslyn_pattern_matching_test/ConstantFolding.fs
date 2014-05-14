module ConstantFolding

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open AstPatterns
open Shared

type SymbolId = 
    | SymbolId of int

type StringSymbol = 
    | StringSymbol of SymbolId

//type BooleanSymbol = 
//    | BooleanSymbol of int
type StringExpression<'s> = 
    | Constant of string
    | Symbol of 's
    | Phi of 's * 's
    | Expression of ExpressionSyntax

type RoslynSymbolId = 
    | RoslynSymbolId of string * Start : int * End : int

type SsaSymbolId = 
    | SsaSymbolId of int * HumanReadableName : string

type Statement = 
    | StringAssignment of StringSymbol * list<StringExpression<SsaSymbolId>>

type SsaSymbolGeneratorState = 
    | SsaSymbolGeneratorState of int

let symbolKey (semantic : SemanticModel) (s : ISymbol) = 
    let sourceSpan = (s.Locations |> Seq.head).SourceSpan
    RoslynSymbolId((s.ToDisplayString()), (sourceSpan.Start), (sourceSpan.End))

let nextSymbolIdFromName (SsaSymbolGeneratorState i) name = SsaSymbolId(i, name), SsaSymbolGeneratorState(i + 1)
let nextSymbolId (state : SsaSymbolGeneratorState) (s : ISymbol) = nextSymbolIdFromName state (s.Name)
let newSymbolGenerator = SsaSymbolGeneratorState(0)

let rec flattenStringConcatExpression (semantic : SemanticModel) (e : ExpressionSyntax) = 
    match e with
    | StringLiteral(s0) -> [ Constant s0 ]
    | IdentifierName(id) -> 
        let symbol = semantic.GetSymbolInfo(id).Symbol
        [ Symbol(symbolKey semantic symbol) ]
    | Add(l, r) -> (flattenStringConcatExpression semantic l) @ (flattenStringConcatExpression semantic r)
    | _ -> [ Expression e ]

let mapExpressionToSsaSymbols symbols init = 
    init |> List.map (fun entry -> 
                match entry with
                | Constant c -> Constant c
                | Expression e -> Expression e
                | Symbol s -> 
                    symbols
                    |> Map.find s
                    |> Symbol)

//map Roslyn symbols in these expressions to SSA'ed symbols
let mapStringExpressions (semantic : SemanticModel) (activeSymbols : Map<RoslynSymbolId, SsaSymbolId>) 
    (expr : ExpressionSyntax) = 
    expr
    |> flattenStringConcatExpression semantic
    |> mapExpressionToSsaSymbols activeSymbols

let build (semantic : SemanticModel) (systemTypes : SystemTypes) (methodNode : MethodDeclarationSyntax) = 
    let (|IsType|) t (n : SyntaxNode) = 
        match (semantic.GetTypeInfo(n).Type) with
        | null -> false
        | evaluatedType when evaluatedType = t -> true
        | _ -> false
    
    let isType (t : ITypeSymbol) (n : SyntaxNode) = semantic.GetTypeInfo(n).Type = t
    let isStringType = isType (systemTypes.StringType)
    let isBooleanType = isType (systemTypes.BooleanType)
    let symbolKey = symbolKey semantic
    let mapStringExpressions = mapStringExpressions semantic
    
    let rec processStatements symbolsMap symbolValues idGenState (statements : list<StatementSyntax>) = 
        statements |> Seq.fold (fun ((symbolsMap, symbolExpressions), ids) statement -> 
                          match statement with
                          | LocalDeclarationStatement(t, decls) -> 
                              decls 
                              |> Seq.fold 
                                     (fun ((symbols, symbolExpressions), idGenState) decl -> 
                                     let mappedInit = decl.Initializer.Value |> mapStringExpressions symbols
                                     let symbol = semantic.GetDeclaredSymbol(decl)
                                     let symbolKey = symbolKey symbol
                                     let ssaId, ids' = nextSymbolId idGenState symbol
                                     (symbols |> Map.add symbolKey ssaId, symbolExpressions |> Map.add ssaId mappedInit), 
                                     ids') ((symbolsMap, symbolExpressions), ids)
                          | ExpressionStatement(AddAssignment(IdentifierName(id), r)) -> 
                              let symbol = semantic.GetSymbolInfo(id).Symbol
                              let symbolKey = symbolKey symbol
                              let nextId, updatedIdGen = nextSymbolId ids symbol
                              let currentSsaSymbol = symbolsMap |> Map.find symbolKey
                              let mappedInit = Symbol currentSsaSymbol :: (r |> mapStringExpressions symbolsMap)
                              let ssaId, ids' = nextSymbolId ids symbol
                              (symbolsMap |> Map.replace symbolKey ssaId, symbolExpressions |> Map.add ssaId mappedInit), 
                              ids'
                          | ExpressionStatement(SimpleAssignmentExpression(IdentifierName(id), r)) -> 
                              let symbol = semantic.GetSymbolInfo(id).Symbol
                              let mappedInit = r |> mapStringExpressions symbolsMap
                              let symbolKey = symbolKey symbol
                              let ssaId, ids' = nextSymbolId ids symbol
                              (symbolsMap |> Map.replace symbolKey ssaId, symbolExpressions |> Map.add ssaId mappedInit), 
                              ids'
                          | IfStatement(cond, ifStmts, elseStmts) -> 
                              let (as0, sv0), ids' = ifStmts |> processStatements symbolsMap symbolExpressions ids
                              let (as1, sv1), ids'' = elseStmts |> processStatements symbolsMap symbolExpressions ids'
                              let preupdateSymbols = Map.keys symbolsMap
                              let symbolsUpdatedInIfBranch = Map.findKeysWithDifferentValues as0 symbolsMap
                              let symbolsUpdatedInElseBranch = Map.findKeysWithDifferentValues as1 symbolsMap
                              let symbolsUpdatedInIfOrElse = 
                                  Set.union symbolsUpdatedInIfBranch symbolsUpdatedInElseBranch
                              let symbolsUpdatedInBothBranches = 
                                  Set.intersect symbolsUpdatedInIfBranch symbolsUpdatedInElseBranch
                              //roslyn symbol, if branch ssa symbol, else branch ssa symbol
                              let symbolsUpdatedInBothBranchesAndFinalSsaSymbolInBothBranches = 
                                  symbolsUpdatedInBothBranches 
                                  |> Set.map (fun k -> (k, as0 |> Map.find k, as1 |> Map.find k))
                              let symbolsOnlyUpdatedInIfBranch = 
                                  Set.difference symbolsUpdatedInIfBranch symbolsUpdatedInBothBranches
                              let symbolsOnlyUpdatedInElseBranch = 
                                  Set.difference symbolsUpdatedInElseBranch symbolsUpdatedInBothBranches
                              let symbolsNotUpdatedInEitherBranches = 
                                  Set.difference preupdateSymbols symbolsUpdatedInIfOrElse
                              
                              let makePhiSymbols ids symbolsUpdatedInBothBranchesAndFinalSsaSymbolInBothBranches = 
                                  symbolsUpdatedInBothBranchesAndFinalSsaSymbolInBothBranches 
                                  |> Seq.fold 
                                         (fun (symbolsMap, symbolExpressions, ids) ((RoslynSymbolId(name, _, spanEnd) as s), ifSsaSymbol, elseSsaSymbol) -> 
                                         let ssaId, ids' = nextSymbolIdFromName ids name
                                         let initExpr = [Phi(ifSsaSymbol, elseSsaSymbol)]
                                         symbolsMap |> Map.add s ssaId, symbolExpressions |> Map.add ssaId initExpr, 
                                         ids') (Map.empty, Map.empty, ids)
                              
                              let symbolsMapToAdd, symbolExpressionsToAdd, ids''' = 
                                  makePhiSymbols ids'' symbolsUpdatedInBothBranchesAndFinalSsaSymbolInBothBranches
                              
                              //plz refactor this out!
                              let union4 m0 m1 m2 m3 = 
                                  (m0
                                   |> Map.union m1
                                   |> Map.union m2
                                   |> Map.union m3)

                              
                              
                              let updatedSymbolExpressions = union4 symbolExpressions sv0 sv1 symbolExpressionsToAdd
                              
                              let updatedSymbolsMap = 
                                  [ symbolsMap |> Map.filterKeys symbolsNotUpdatedInEitherBranches
                                    as0 |> Map.filterKeys symbolsOnlyUpdatedInIfBranch
                                    as1 |> Map.filterKeys symbolsOnlyUpdatedInElseBranch
                                    symbolsMapToAdd ]
                                  |> List.map (fun x -> x |> Map.toList)
                                  |> List.collect id
                                  |> Map.ofList
                              (updatedSymbolsMap, updatedSymbolExpressions), ids'''
                          (*
                               general logic:

                               When taking 2 branches, 
                               - if no branch assigns to a variable, then we can leave the symbol as it is
                               - if one branch assigns to a variable but the other doesn't, then the assigned variable after ssa is the new var
                               - if both branch assigns to a variable, then we have to have a phi function 
                               *)
                          | _ -> (symbolsMap, symbolExpressions), ids) ((symbolsMap, symbolValues), idGenState)
    methodNode.Body.Statements
    |> Seq.toList
    |> processStatements Map.empty Map.empty newSymbolGenerator

let evaluate (symbolValues : Map<SsaSymbolId, list<StringExpression<SsaSymbolId>>>) = 
    let rec vals= 
        Shared.memoize (fun k -> 
            let str = 
                symbolValues 
                |> Map.find k            
                |> List.map (fun expr -> 
                       match expr with
                       | Constant c -> c
                       | Symbol s -> vals s
                       | _ -> failwith "unsupported")
                |> Shared.joinString ""
            str)
    symbolValues |> Map.map (fun k _ -> vals k)
