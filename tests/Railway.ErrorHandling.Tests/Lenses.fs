namespace Railway.ErrorHandling.Tests.Lenses

module Example1 =
    type Postcode = Postcode of string
    type Address = { Postcode: Postcode }
    type Location = { Address : Address }
    type Building = { Location : Location;  Name : string }
    
    let building =
        {
            Location =
                {
                    Address =
                        { Postcode = Postcode "AB" }
                }
            Name = "Name"
        }
        
    let postcode =
        building.Location.Address.Postcode
        
    let newPostcode = Postcode "E1"
    
    let newAddress =
        { building.Location.Address with Postcode = newPostcode }
        
    let newBuilding =
        { building with Name = "New name" }
        
        
module Example2 =
    open Example1
    
    let getPostcode =
        fun address -> address.Postcode
        
    let getAddress =
        fun location -> location.Address
        
    let getPostcodeOfAddress =
        getAddress >> getPostcode
        
    let setPostcode =
        fun newPostcode address -> {address with Postcode = newPostcode}
        
module SimpleLens=
    type Lens<'a, 'b> =
        ('a -> 'b) * ('b -> 'a -> 'a)
        
module AetherMorphismExample =
    open Aether
    open Aether.Operators

    type Building = { Storeys: string}
        with 
        static member Storeys_ :Lens<_,_> =
            let getter = fun b -> b.Storeys
            let setter = fun s b -> { b with Storeys = s}
            (getter,setter)

    let string2int_ =
        let toInt (s: string) = 
            match System.Int32.TryParse s with
            | true, i -> Some i
            | _ -> None
        let fromInt (i:int) =
            string i
        (toInt, fromInt)

    let storeys_ =
        Building.Storeys_ >-> string2int_

    let building = { Storeys = "2" }

    let storeys = 
        building ^. storeys_

    let newBuilding =
        let newStoreys = 42
        building |> newStoreys ^= storeys_    