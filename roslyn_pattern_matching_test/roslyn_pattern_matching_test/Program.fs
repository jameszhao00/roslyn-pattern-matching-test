
module Main

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open AstPatterns
open Shared

[<EntryPoint>]
let main argv = 
    (*
    let tree = SyntaxFactory.ParseSyntaxTree(@"
	    class Program
	    {
		    static void Foo(int a, string input) {
			        var sql = ""UPDATE patient "";
                var insuranceCarrier = InsuranceRepository.I.GetInsuranceById(insuranceId);
                var isDentalInsurance =
                    insuranceCarrier.SafeGet(i => i.Type) == InsuranceType.Dental;

                
                if (isDentalInsurance)
                {
                    sql += ""INITIAL TRUE"";
                }
                else {
                    sql += ""INITIAL FALSE"";
                }
                if (isDentalInsurance)
                {
                    sql += @"" SET dentalInsuranceCardNumberEncrypted=@insuranceCardNumberEncrypted, dentalInsuranceCardNumberSalt=@insuranceCardNumberSalt, dentalInsuranceGroupNumber=@groupNumber"";
                    if(isDentalInsurance)
                    {
                        sql += "" ISDENTAL TRUE"";
                    }
                    else
                    {
                        sql += "" ISDENTAL FALSE"";
                    }
                }
                else
                {
                    sql += @"" SET healthInsuranceCardNumberEncrypted=@insuranceCardNumberEncrypted, healthInsuranceCardNumberSalt=@insuranceCardNumberSalt, healthInsuranceGroupNumber=@groupNumber"";
                }

                sql += "" WHERE id=@patientId"";

                SqlHelper.ExecuteNonQuery(Sql.GetConnection, sql, new
                {
                    patientId = patient.Id,
                    insuranceCardNumberEncrypted = insuranceCardNumberEncrypted.EncryptedMessage,
                    insuranceCardNumberSalt = new SqlHelper.Blob(insuranceCardNumberEncrypted.IV),
                    groupNumber = insuranceGroupNumber
                });
		    }
	    }")
    *)
    
    
    let tree = SyntaxFactory.ParseSyntaxTree(@"
	    class Program
	    {
		    static void Foo(int abc, string input, bool useCondition) {
                string a = ""foo"";
                string b = ""bbb"";
                if(useCondition)
                {
                    if(abc == 0)
                    {
                        a = a + ""d0"";
                    }
                    else
                    {
                        a = a + ""d1"";
                        b = b + ""ccc"";
                    }
                    
                }
                else {
                    a = a + ""e"";
                }               
                a = a + ""f"";
                a = a + b;
		    }
	    }")
    
    let root = tree.GetRoot()
    let compilation = 
        CSharpCompilation.Create
            ("MyCompilation", syntaxTrees = [| tree |], 
                references = [| new MetadataFileReference(typedefof<System.Object>.Assembly.Location) |])
    let semantic = compilation.GetSemanticModel(tree)
    let methodNode = 
        root.DescendantNodes()
        |> Seq.choose (fun x -> 
                match x with
                | :? MethodDeclarationSyntax as m -> Some m
                | _ -> None)
        |> Seq.exactlyOne
    
    let targetSymbol = 
        let id = 
            methodNode.Body.Statements
            |> Seq.choose (fun x -> 
                    match x with
                    | LocalDeclarationStatement(t, decls) -> (Seq.head >> Some) decls
                    | _ -> None)
            |> Seq.head
        semantic.GetDeclaredSymbol(id)
    
    //let v = Experiment1.allSqlStrings semantic methodNode targetSymbol
    let (activeSymbols, symbolExpressions), idgen = ConstantFolding.build semantic (AstPatterns.specialTypes compilation) methodNode
    activeSymbols |> printMap "active symbols"
    symbolExpressions |> printMap "symbol expressions"
    ConstantFolding.evaluate symbolExpressions |> printMap "evaluated symbols"
    0 // return an integer exit code
