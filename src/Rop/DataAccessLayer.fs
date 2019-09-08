module FCustomer.DataAccessLayer
open FCustomer.DomainModel
open FCustomer.DomainPrimitiveTypes
open FCustomer.Result
open FCustomer.SqlDatabase

type ICustomerDao =
    abstract GetAll : unit -> RopResult<Customer seq, DomainMessage>
    
    abstract GetById : CustomerId.T -> RopResult<Customer, DomainMessage>
    
    abstract Upsert : Customer -> RopResult<unit, DomainMessage>
    
let fomDbCustomer (dbCustomer: DbCustomer) =
    if dbCustomer = null then
        fail SqlCustomerIsINvalid
    else
        let idOrError = createCustomerId dbCustomer.Id
        let firstNameOrError = createFirstName dbCustomer.FirstName
        let lastNameOrError = createLastName dbCustomer.LastName
        let personNameOrError = createPersonalName
                                <!> firstNameOrError
                                <*> lastNameOrError
        let personNameOrError' = FCustomer.Result.lift2R createPersonalName firstNameOrError lastNameOrError
        let emailOrError = createEmail dbCustomer.Email
        
        let customerOrError = createCustomer
                              <!> idOrError
                              <*> personNameOrError
                              <*> emailOrError
                              
        let customerOrError' = FCustomer.Result.lift3R createCustomer idOrError personNameOrError emailOrError
        
        customerOrError

let fromDbCustomerIdomatic (dbCustomer: DbCustomer) =
    if dbCustomer = null then
        fail SqlCustomerIsINvalid
        
    else
        let nameOrError =
            createPersonalName 
            <!> createFirstName dbCustomer.FirstName
            <*> createLastName dbCustomer.LastName
            
        createCustomer
        <!> createCustomerId dbCustomer.Id
        <*> nameOrError
        <*> createEmail dbCustomer.Email
        
        
let toDbCustomer ( customer: Customer) =
    let customerId = customer.Id |> CustomerId.apply id
    let dbCustomer = DbCustomer()
    dbCustomer.Id <- customerId
    dbCustomer.FirstName <- customer.Name.FirstName |> String10.apply id
    dbCustomer.LastName <- customer.Name.LastName |> String10.apply id
    dbCustomer.Email <- customer.Email |> EmailAddress.apply id
    dbCustomer
    
let (|KeyNotFound|DuplicateKey|Timeout|Other|) (ex:SqlException) =
        match ex.Data0 with
        | "KeyNotFound" -> KeyNotFound
        | "DuplicateKey" -> DuplicateKey
        | "Timeout" -> Timeout
        | _ -> Other
        
let failureFromException (ex: SqlException) =
    match ex with
    | Timeout ->
        fail DatabaseTimeout
    | _ ->
        fail (DatabaseError ex.Message)
    
type CustomerDao() =
    interface ICustomerDao with
        member this.GetAll() =
            let db = DbContext()
            let fSuccess (x , _) = Some x
            let fFailure _ = None
            
            try
                db.Customers()
                 |> Seq.map fromDbCustomerIdomatic
                 |> Seq.choose (either fSuccess fFailure)
                 |> succeed
            with
            | :? SqlException as ex -> failureFromException ex
            
        member this.GetById customerId =
            let db = new DbContext()
            let custId = customerId |> CustomerId.apply id
            
            try
                db.Customers()
                |> Seq.tryFind (fun sql -> sql.Id = custId)
                |> Option.map fromDbCustomerIdomatic
                |> failIfNoneR CustomerNotFound
            with
            | :? SqlException as ex -> failureFromException ex
            
        member this.Upsert (customer:Customer) =
            let db = DbContext()
            
            try
                let newDbCustomer = toDbCustomer customer
                let customerId = customer.Id |> CustomerId.apply id
                let existingDbCustomer =
                    db.Customers()
                    |> Seq.tryFind (fun sql -> sql.Id = customerId)
                    
                match existingDbCustomer with
                | None ->
                    db.Insert(newDbCustomer)
                    succeed()
                | Some c ->
                    db.Update(newDbCustomer)
                    if newDbCustomer.Email <> c.Email then
                        let event = EmailAddressChanged(c.Email, newDbCustomer.Email)
                        succeedWithMsg () event
                    else
                        succeed()
            with
            | :? SqlException as ex -> failureFromException ex
            
            
        
                                