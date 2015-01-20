
let log x =
    printfn "Value: %A" x
    x

let doStuff _ =
    let four = 3 + 1 |> log
    let seven = four + 3 |> log
    seven + 11 |> log

type LogBuilder() =
    member x.Bind(value, continuation) =
        log value |> ignore
        continuation value
    member x.Return(value) =
        printfn "Return: %A" value |> ignore
        value

let logging = new LogBuilder()

let doStuff2 _ =
    logging {
        let! four = 3 + 1
        let! seven = four + 3
        let noLog = seven + 8
        
        return seven + 11
    }

// doStuff()
doStuff2()