[<AutoOpen>]
module Cin =
    let private q = System.Collections.Generic.Queue()

    let cin _ =
        while q.Count = 0 do
            for s in stdin.ReadLine().Split() do
                if s <> "" then q.Enqueue s
        q.Dequeue()
