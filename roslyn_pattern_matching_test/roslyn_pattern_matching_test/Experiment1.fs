(* 
- find the target symbol usage
- walk backwards from the usage and find all assignments to it
- from this graph find all possible values of the symbol at the usage

This doesn't work well because going it's non-intuitive as to how we can let other variables
participate in the evaluation (i.e. we only gather the dataflow for a single variable)

I'm going to try to do SSA + full dataflow next
*)

module Experiment1
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open AstPatterns

    type Condition = 
        | Condition of ExpressionSyntax

    type Symbol = 
        | Self
        | Other of ISymbol

    type StringPart = 
        | Symbol of Symbol
        | Value of string

    type Assignment = 
        | Initialization of list<StringPart>
        | Conditional of Condition * passed : list<Assignment> * failed : list<Assignment>



    let allSqlStrings (semantic:SemanticModel) (methodNode:MethodDeclarationSyntax) (targetSymbol:ISymbol) =     
        let assignmentsToSymbol = 
            let rec flattenStringConcatExpression (selfId) (semantic : SemanticModel) (e : ExpressionSyntax) = 
                match e with
                | StringLiteral(s0) -> [ Value s0 ]
                | IdentifierName(id) -> 
                    let symbol = semantic.GetSymbolInfo(id).Symbol
                    if symbol = selfId then [ Symbol Self ]
                    else [ Symbol(Other symbol) ]
                | Add(l, r) -> 
                    (flattenStringConcatExpression selfId semantic l) @ (flattenStringConcatExpression selfId semantic r)
                | _ -> failwith "Unexpected node"
        
            let rec extractAssignments stmt = 
                stmt
                |> Seq.toList
                |> List.rev
                |> Seq.fold (fun acc stmt -> 
                       match stmt with
                       | ExpressionStatement(AddAssignment(IdentifierName(id), r) as aa) -> 
                           match (semantic.GetSymbolInfo(id).Symbol) = targetSymbol with
                           | true -> ((Symbol Self) :: (flattenStringConcatExpression targetSymbol semantic r)
                                      |> Initialization) :: acc
                           | false -> acc
                       | ExpressionStatement(SimpleAssignmentExpression(IdentifierName(id), r) as ae) -> 
                           match (semantic.GetSymbolInfo(id).Symbol) = targetSymbol with
                           | true -> (flattenStringConcatExpression targetSymbol semantic r |> Initialization) :: acc
                           | false -> acc
                       | LocalDeclarationStatement(t, decls) -> 
                           let candidates = 
                               decls
                               |> Seq.filter (fun x -> semantic.GetDeclaredSymbol(x) = targetSymbol)
                               |> Seq.toList
                           match candidates with
                           | [ VariableDeclarator(id, StringLiteral(init)) ] -> (Initialization [ Value(init) ]) :: acc
                           | _ -> acc
                       | IfStatement(cond, el, stmts) -> 
                           Conditional(Condition cond, extractAssignments stmts, extractAssignments el) :: acc
                       | _ -> acc) []
        
            extractAssignments (methodNode.Body.Statements |> Seq.toList)
    
        //from a list of assignments, generate all possible sql strings (from branching)

        (*
        TODO: currently this solution doesn't handle (or if the value evaluation is in the if statement)

        var sql = "SELECT * FROM ABC";
        if(a)
        {
            sql += " WHERE FOO = 1";
            CreateList<DTO>(sql);
        }
        *)
        let rec allSqlStrings (assignments : list<Assignment>) (baseStrings : list<string>) : list<string> = 
            baseStrings |> List.collect (fun baseString -> 
                               assignments |> List.fold (fun acc assignment -> 
                                                  match assignment with
                                                  | Initialization(xs) -> 
                                                      acc 
                                                      |> List.map 
                                                             (fun existing -> 
                                                             xs 
                                                             |> Seq.fold 
                                                                    (fun joinedParts part -> 
                                                                    match part with
                                                                    | Symbol Self -> joinedParts + existing
                                                                    | Value s -> joinedParts + s
                                                                    | Symbol _ -> 
                                                                        failwith 
                                                                            "References to other symbols not implemented") 
                                                                    "")
                                                  | Conditional(_, passed, failed) -> 
                                                      let passedSqlStrings = allSqlStrings passed acc
                                                      let failedSqlStrings = allSqlStrings failed acc
                                                      passedSqlStrings @ failedSqlStrings) [ baseString ])
    
        allSqlStrings assignmentsToSymbol [ "" ]
