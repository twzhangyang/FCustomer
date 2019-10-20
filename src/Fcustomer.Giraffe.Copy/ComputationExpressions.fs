module FCustomer.Giraffe.Copy.ComputationExpressions

type OptionBuilder() =
    member __.Bind(v, f) = Option.bind f v
    member __.Return v = Some v
    member __.ReturnFrom v = v
    member __.Zero = None
    
let opt = OptionBuilder()

type ResultBuilder() =
    member __.Bind(v, f) = Result.bind f v
    member __.Return v = Ok v
    
let res = ResultBuilder()


    
