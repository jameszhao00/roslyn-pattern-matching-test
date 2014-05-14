module AstPatterns

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

let (|Add|_|) (n : SyntaxNode) = 
    match n with
    | :? BinaryExpressionSyntax as s when s.CSharpKind() = SyntaxKind.AddExpression -> Some(s.Left, s.Right)
    | _ -> None

let (|AddAssignment|_|) (n : SyntaxNode) = 
    match n with
    | :? BinaryExpressionSyntax as s when s.CSharpKind() = SyntaxKind.AddAssignmentExpression -> Some(s.Left, s.Right)
    | _ -> None

let (|VariableDeclarator|_|) (n : SyntaxNode) = 
    match n with
    | :? VariableDeclaratorSyntax as s -> Some(s.Identifier, s.Initializer.Value)
    | _ -> None

let (|StringLiteral|_|) (n : SyntaxNode) = 
    match n with
    | :? LiteralExpressionSyntax as s when s.CSharpKind() = SyntaxKind.StringLiteralExpression -> 
        Some(s.Token.ValueText)
    | _ -> None

let (|LocalDeclarationStatement|_|) (n : SyntaxNode) = 
    match n with
    | :? LocalDeclarationStatementSyntax as s -> Some(s.Declaration.Type, s.Declaration.Variables)
    | _ -> None

let (|ExpressionStatement|_|) (n : SyntaxNode) = 
    match n with
    | :? ExpressionStatementSyntax as s -> Some(s.Expression)
    | _ -> None

let (|SimpleAssignmentExpression|_|) (n : SyntaxNode) = 
    match n with
    | :? BinaryExpressionSyntax as s when s.CSharpKind() = SyntaxKind.SimpleAssignmentExpression -> 
        Some(s.Left, s.Right)
    | _ -> None

let (|ElseClause|_|) (n : SyntaxNode) = 
    match n with
    | :? ElseClauseSyntax as s -> Some(s.Statement)
    | _ -> None

let (|Block|_|) (n : SyntaxNode) = 
    match n with
    | :? BlockSyntax as s -> Some(s.Statements)
    | _ -> None

let flattenIfBlock (n : StatementSyntax) = 
    match n with
    | Block(stmts) -> stmts |> Seq.toList
    | _ -> [ n ]

let (|IfStatement|_|) (n : SyntaxNode) = 
    match n with
    | :? IfStatementSyntax as s -> 
        //flatten out the statements, since we can either have a statement or a block
        let elseStmts = 
            match s.Else with
            | null -> []
            | _ -> s.Else.Statement |> flattenIfBlock
        Some(s.Condition, s.Statement |> flattenIfBlock, elseStmts)
    | _ -> None

let (|IdentifierName|_|) (n : SyntaxNode) = 
    match n with
    | :? IdentifierNameSyntax as s -> Some(s)
    | _ -> None

type SystemTypes = 
    { StringType : ITypeSymbol
      BooleanType : ITypeSymbol }

let specialTypes (compilation : Compilation) = 
    { SystemTypes.StringType = compilation.GetSpecialType(SpecialType.System_String)
      SystemTypes.BooleanType = compilation.GetSpecialType(SpecialType.System_Boolean) }


