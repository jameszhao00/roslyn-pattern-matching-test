module Shared

let printMap name m = 
    printfn "%s:" name
    m |> Map.iter (fun k v -> printfn "%A -> %A" k v)
    printfn ""

//from http://en.wikibooks.org/wiki/F_Sharp_Programming/Caching
let memoize f = 
    let dict = new System.Collections.Generic.Dictionary<_, _>()
    fun n -> 
        match dict.TryGetValue(n) with
        | (true, v) -> v
        | _ -> 
            let temp = f (n)
            dict.Add(n, temp)
            temp

let joinString (sep : string) (xs : list<string>) = System.String.Join(sep, xs |> List.toArray)

module Map = 
    let replace k v' m = 
        m
        |> Map.remove k
        |> Map.add k v'
    
    //given two maps, find keys in that are in both m0 and m1 and have different values
    let findKeysWithDifferentValues m0 m1 = 
        let sharedKeys = Set.intersect (m0 |> Map.keys) (m1 |> Map.keys)
        sharedKeys |> Set.filter (fun k -> (m0 |> Map.find k) <> (m1 |> Map.find k))
