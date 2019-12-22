module Railway.ErrorHandling.Tests.Guidelines
open System
open Railway.ErrorHandling.Tests

type Point =
    abstract DistanceTo : otherPoint: Point -> float

type Counter() =
    let mutable count = 0
    
    member this.Next() =
        count <- count + 1
        count
        
    
type Serializer =
    abstract Serialize<'T> : preserveRefEq : bool -> value: 'T -> string
    abstract Deserialize<'T> : preserveRefEq : bool -> pickle: string -> 'T

type Serializer<'T> = {
    Serialize: bool -> 'T -> string
    Deserialize: bool -> string -> 'T
}    

type Vector(x : float) =
    member v.X = x
    static member (*) (vector: Vector, scalar: float) = Vector(vector.X * scalar)
    static member (+) (vector1: Vector, vector2: Vector) = Vector(vector1.X + vector2.X)
    
    [<CompiledName("Create")>]
    static member Create x = Vector(x)
    
let v = Vector(10.0)
let u = Vector(12.0)
let vu = v + u

type Logger =
    member this.Log(message) = 2
    
    member this.Log(message, retry: int) = 3
    
type BST<'T> =
    | Empty
    | Node of 'T * BST<'T> * BST<'T>
    
    
type Point1(angle, radius) =
    new () = Point1(angle = 10.0, radius = 20.0)
    
    member x.Angle = angle
    member x.Radius = radius
    member x.Stretch(l) = Point1(angle, radius * l)
    member x.Wrap(f) = Point1(f(radius), radius)
    static member Circle(n) =
        [for i in 1..n -> Point1(angle=2.0 * Math.PI / float(n), radius=1.0)]
        
type IPrintable =
    abstract member Print : format : string -> unit

type SomeClass(x: int, y: float) =
    interface IPrintable with
        member x.Print(format: string) = printfn "hello";
        

let a = SomeClass(2, 2.0)
let a' = (a :> IPrintable).Print("a")

type SomeClass2(x: int, y: float) =
    member x.Print(format: string) = (x :> IPrintable).Print(format)
    
    interface IPrintable with
        member x.Print format = printfn "hello"
    
    
let b' = SomeClass2(2, 2.).Print("a")

let makePrintable(x: int, y: float) =
    { new IPrintable with
            member this.Print format = printfn "hello"
    }
    
let x3 = makePrintable(1, 2.0)
x3.Print("hello")

type Interface1 =
    abstract member Method1: int -> int
    
type Interface2 =
    abstract member Method2: int -> int
    
    abstract member Area: float with get,set
    
    
type MyClass() =
    let mutable area = 1.0
    interface Interface1 with
        member x.Method1 a = 2
    
    interface Interface2 with
        member x.Method2 b = 3
        member x.Area
            with get() = area
            and set(value) = area <- value
        
[<AbstractClass>]
type AbstractBase() =
    abstract Property1 : int with get, set
    
type Derived1() =
    inherit AbstractBase()
    let mutable value = 10
    override this.Property1 with get() = value and set(v) = value <- v
    
type Base1() =
    let mutable value = 10
    abstract Property1 : int with get, set
    default this.Property1 with get() = value and set(v) = value <- v
    member val Property2 = value with get
    
type Base2() =
    inherit AbstractBase()
    let mutable value = 10
    override x.Property1 with get() = value and set(v) = value <- v
    
let base1 = Base1()
let p1 = base1.Property2
base1.Property1 <- 2

type MyClass1(property1: int) =
    member val Property1 = property1
    member val Property2 = "" with get, set    
    
let myClass1 = MyClass1(2)
let p = myClass1.Property1


type MyClass2(x0, y0, z0) =
    let mutable x = x0
    let mutable y = y0
    let mutable z = z0
    do
        printfn "Initialized object that has coordinates (%d, %d, %d)" x y z
    
    member this.X with get() = x and set(v) = x <- v
    member this.Y with get() = y and set(v) = y <- v
    member val Z = z with get, set
    
    new (x0) = MyClass2(x0, 2, 3)
    
    
type Person(name: string, id: Guid) =
    let mutable nameIn = name
    let mutable IdIn = id
    do
        printfn "Initialized person with (%s, %A)" name id
        
    member val Name = nameIn with get, set
    member this.Id with get() = IdIn and set(id) = IdIn <- id
    new () =
        Person("perter", Guid.NewGuid())
        then
            printfn "Initialized person with %s, %A" "perter", "hello"
            
            

            
    

    
    


    

    

    


    

    