[<AutoOpen>]
module Cin =
    let private q = System.Collections.Generic.Queue()

    let inline cin _ =
        while q.Count = 0 do
            for s in stdin.ReadLine().Split() do
                if s <> null && s <> "" then q.Enqueue s
        q.Dequeue()
