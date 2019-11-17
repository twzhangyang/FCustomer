namespace Railway.ErrorHandling.CardGame

module Card =
    type Suit = | Club | Diamond | Spade | Heart

    type Rank = | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

    type Card = Suit * Rank

    type Hand = Card list

    type Deck = Card list

    type Player = { Name: string; Hand: Hand }

    type Game = { Deck: Deck; Players: Player list }

module Contact =
    type String1 = String1 of string
    type String50 = String50 of string

    type EmailAddress =
        EmailAddress of string

    type VerifiedEmail =
        EmailAddress of EmailAddress

    type EmailContactInfo =
        | Unverified of EmailAddress
        | Verified of VerifiedEmail

    type PersonalContactInfo =
        {
        address1: string
        address2: string
        address3: string
        address4: string
        country: string
        }

    type ContactInfo =
        | EmailOnly of EmailContactInfo
        | AddressOnly of PersonalContactInfo
        | EmailAndAddr of EmailAddress * PersonalContactInfo

    type PersonalName =
        {
        FirstName: String50
        MiddleInitial: String1 option
        LastName: String50
        }

    type Contact = {
    Name: PersonalName
    ContactInfo: ContactInfo
    }

module Payment =
    type CardType = | Visa | Mastercard
    type CardNumber = CardNumber of string
    type ChequeNumber = ChequeNumber of int
    type EmailAddress = EmailAddress of string
    type BitcoinAddress = BitcoinAddress of string

    type PaymentMethod =
        | Cash
        | Cheque of ChequeNumber
        | Card of CardType * CardNumber
        | PayPal of EmailAddress
        | Bitcoin of BitcoinAddress

    type PaymentAmount =
        PaymentAmount of float

    type Payment = {
        PaymentMethod: PaymentMethod
        PaymentAmount: PaymentAmount
    }

    let cardTypeToString cardType =
        match cardType with
        | Visa -> sprintf "visa"
        | Mastercard -> sprintf "master card"

    let chequeNumberToInt =
        function
            | ChequeNumber number -> number

    let paymentMethod1 = Cash
    let paymentMethod2 = Cheque(ChequeNumber 42)
    let paymentMethod3 = Card(Visa, CardNumber("number"))

    let makePayment amount paymentMethod =
        {
          PaymentMethod = paymentMethod;
          PaymentAmount = amount
        }
        
    let payment1 = makePayment (PaymentAmount 10.2) (paymentMethod2)
    
    




