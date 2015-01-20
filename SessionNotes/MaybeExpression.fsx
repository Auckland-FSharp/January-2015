
type Cat = { Name: string }
type Person = { First: string; Last: string; Pet: Option<Cat> }



let getPerson name =
    match name with
    | "Bob" -> Some { First = "Bob"; Last = "Jones"; Pet = None}
    | "Trev" -> Some { First = "Trev"; Last = "Bayliss"; Pet = Some { Name = "Ugly" } }
    | _ -> None

let getPetOfPerson personName =
    let person = getPerson personName
    if person.IsSome then person.Value.Pet else None


type LogBuilder() =
    member x.Bind(value, continuation) =
        log value |> ignore
        continuation value
    member x.Return(value) =
        printfn "Return: %A" value |> ignore
        value

let logging = new LogBuilder()



type Maybe2Builder() =
    member x.Bind (optionalValue:Option<'T>, continuation:('T -> Option<'U>)) =
        logging {
            if optionalValue.IsSome then
                return continuation optionalValue.Value
            else
                return Option<'U>.None
        }

    member x.Return (value:'T) =
        Some value

let maybe2 = new Maybe2Builder()



let getNameOfPet name =
    maybe2 {
        let! person = getPerson name
        let! pet = person.Pet
        return pet.Name
    }

getNameOfPet "Bob"