[<AutoOpen>]
module MyPriorityQueue =
    open System.Collections
    open System.Collections.Generic

    type MyPriorityQueue<'a>(?capacity, ?comparer) =
        let heap =
            match capacity with
            | Some (x: int) -> List<'a> x
            | None -> List()
        let mutable size = 0
        let comparer =
            match comparer with
            | Some cf -> cf
            | None -> fun a b -> Comparer.Default.Compare(a, b)
        let rec up pos =
            let elem = heap.[pos]
            let mutable pos, flg = pos, true
            while pos > 0 && flg do
                let parentPos = (pos - 1) / 2
                match comparer elem heap.[parentPos] <= 0 with
                | true -> flg <- false
                | false ->
                    heap.[pos] <- heap.[parentPos]
                    pos <- parentPos
            heap.[pos] <- elem
        let down pos =
            let elem = heap.[pos]
            let mutable pos, childPos, flg = pos, pos * 2 + 1, true
            while childPos < size && flg do
                if childPos + 1 < size then
                    childPos <-
                        match comparer heap.[childPos] heap.[childPos + 1] with
                        | x when x >= 0 -> childPos
                        | _ -> childPos + 1
                match comparer elem heap.[childPos] >= 0 with
                | true -> flg <- false
                | false ->
                    heap.[pos] <- heap.[childPos]
                    pos <- childPos
                    childPos <- pos * 2 + 1
            heap.[pos] <- elem
        interface IEnumerable<'a> with
            member _.GetEnumerator() = heap.GetEnumerator() :> IEnumerator<'a>
            member _.GetEnumerator() = heap.GetEnumerator() :> IEnumerator
        member _.Count = size
        member _.First = heap.[0]
        member _.TrimExcess() =
            if size < heap.Count then
                heap.RemoveRange(size, heap.Count - size)
        member _.Enqueue elememt =
            let pos = size
            match heap.Count <= size with
            | true -> heap.Add(elememt)
            | false -> heap.[pos] <- elememt
            size <- size + 1
            up pos
        member _.Dequeue() =
            let res = heap.[0]
            heap.[0] <- heap.[size - 1]
            size <- size - 1
            down 0
            res

open MyPriorityQueue
