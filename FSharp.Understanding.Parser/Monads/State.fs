module FSharp.Understanding.Parser.Monads.State

type Label = string
type VitalForce = { units: int }

let getVitalForce vitalForce =
    let oneUnit = { units = 1 }
    let remaining = { units = vitalForce.units - 1 }
    
    oneUnit, remaining

type DeadLeftLeg = DeadLeftLeg of Label
type LiveLeftLeg = LiveLeftLeg of Label * VitalForce

let makeLiveLeftLeg (deadLeftLeg, vitalForce) =
    let (DeadLeftLeg label) = deadLeftLeg
    let oneUnit, remainingUnit = getVitalForce vitalForce
    let liveLeftLeg = LiveLeftLeg (label, oneUnit)
    
    liveLeftLeg, remainingUnit
    
let makeLiveLeftLeg' deadLeftLeg =
    let becomeAlive vitalForce =
        let (DeadLeftLeg label) = deadLeftLeg
        let oneUnit, remainingUnit = getVitalForce vitalForce
        let liveLeftLeg = LiveLeftLeg (label, oneUnit)
        liveLeftLeg, remainingUnit
    becomeAlive
    
type M<'LiveBodyPart> =
    M of (VitalForce -> 'LiveBodyPart * VitalForce)
    
let makeLiveLeftLegM deadLeftLeg =
    let becomeAlive vitalForce =
        let (DeadLeftLeg label) = deadLeftLeg
        let oneUnit, remainingUnit = getVitalForce vitalForce
        let liveLeftLeg = LiveLeftLeg (label, oneUnit)
        liveLeftLeg, remainingUnit
    M becomeAlive
    
let deadLeftLeg = DeadLeftLeg "leg1"
let liveLeftLeg = makeLiveLeftLegM deadLeftLeg

let runM (M f) vitalForce =
    f vitalForce
let vf = { units = 10 }

let liveLeftLeg' = runM liveLeftLeg vf

type DeadLeftBrokenArm = DeadLeftBrokenArm of Label 

// A live version of the broken arm.
type LiveLeftBrokenArm = LiveLeftBrokenArm of Label * VitalForce

// A live version of a heathly arm, with no dead version available
type LiveLeftArm = LiveLeftArm of Label * VitalForce

// An operation that can turn a broken left arm into a heathly left arm
type HealBrokenArm = LiveLeftBrokenArm -> LiveLeftArm

// implementation of HealBrokenArm
let healBrokenArm (LiveLeftBrokenArm (label,vf)) = LiveLeftArm (label,vf)

/// convert a M<LiveLeftBrokenArm> into a M<LiveLeftArm>
let makeHealedLeftArm brokenArmM = 

    // create a new inner function that takes a vitalForce parameter
    let healWhileAlive vitalForce = 
        // run the incoming brokenArmM with the vitalForce 
        // to get a broken arm
        let brokenArm,remainingVitalForce = runM brokenArmM vitalForce 
        
        // heal the broken arm
        let healedArm = healBrokenArm brokenArm

        // return the healed arm and the remaining VitalForce
        healedArm, remainingVitalForce

    // wrap the inner function and return it
    M healWhileAlive  

let makeGenericTransform f brokenArmM = 

    // create a new inner function that takes a vitalForce parameter
    let healWhileAlive vitalForce = 
        let brokenArm,remainingVitalForce = runM brokenArmM vitalForce 
        
        // heal the broken arm using passed in f
        let healedArm = f brokenArm
        healedArm, remainingVitalForce

    M healWhileAlive
    
module M =
    let mapM f bodyPartM = 
        let transformWhileAlive vitalForce = 
            let bodyPart,remainingVitalForce = runM bodyPartM vitalForce 
            let updatedBodyPart = f bodyPart
            updatedBodyPart, remainingVitalForce
        M transformWhileAlive 
    
    let map2M f m1 m2 =
        let becomeAlive vitalForce = 
            let v1,remainingVitalForce = runM m1 vitalForce 
            let v2,remainingVitalForce2 = runM m2 remainingVitalForce  
            let v3 = f v1 v2
            v3, remainingVitalForce2    
        M becomeAlive
    
    let returnM x = 
        let becomeAlive vitalForce = 
            x, vitalForce 
        M becomeAlive
        
    let bindM f bodyPartM = 
        let becomeAlive vitalForce = 
            let bodyPart, remainingVitalForce = runM bodyPartM vitalForce 
            let newBodyPartM = f bodyPart 
            let newBodyPart, remainingVitalForce2 = runM newBodyPartM remainingVitalForce 
            newBodyPart, remainingVitalForce2    
        M becomeAlive
    
    let applyM mf mx =
        let becomeAlive vitalForce = 
            let f,remainingVitalForce = runM mf vitalForce 
            let x,remainingVitalForce2 = runM mx remainingVitalForce  
            let y = f x
            y, remainingVitalForce2    
        M becomeAlive
    
    // map works with options
    let healBrokenArmO = Option.map healBrokenArm
    // LiveLeftBrokenArm option -> LiveLeftArm option

    // map works with lists
    let healBrokenArmL = List.map healBrokenArm

let makeLiveLeftBrokenArm deadLeftBrokenArm = 
    let (DeadLeftBrokenArm label) = deadLeftBrokenArm
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveLeftBrokenArm = LiveLeftBrokenArm (label,oneUnit)
        liveLeftBrokenArm, remainingVitalForce    
    M becomeAlive

/// create a dead Left Broken Arm
let deadLeftBrokenArm = DeadLeftBrokenArm "Victor"

/// create a M<BrokenLeftArm> from the dead one
let leftBrokenArmM = makeLiveLeftBrokenArm deadLeftBrokenArm 
let leftArmM = leftBrokenArmM |> M.mapM healBrokenArm

type DeadRightLowerArm = DeadRightLowerArm of Label 
type DeadRightUpperArm = DeadRightUpperArm of Label
type LiveRightLowerArm = LiveRightLowerArm of Label * VitalForce
type LiveRightUpperArm = LiveRightUpperArm of Label * VitalForce

type LiveRightArm = {
    lowerArm : LiveRightLowerArm
    upperArm : LiveRightUpperArm
    }

let armSurgery lowerArm upperArm =
    {lowerArm=lowerArm; upperArm=upperArm}
    
let makeLiveRightLowerArm (DeadRightLowerArm label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveRightLowerArm = LiveRightLowerArm (label,oneUnit)
        liveRightLowerArm, remainingVitalForce    
    M becomeAlive

let makeLiveRightUpperArm (DeadRightUpperArm label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveRightUpperArm = LiveRightUpperArm (label,oneUnit)
        liveRightUpperArm, remainingVitalForce    
    M becomeAlive

let deadRightLowerArm = DeadRightLowerArm "Tom"
let lowerRightArmM = makeLiveRightLowerArm deadRightLowerArm 

let deadRightUpperArm = DeadRightUpperArm "Jerry"
let upperRightArmM = makeLiveRightUpperArm deadRightUpperArm

let armSurgeryM  = M.map2M armSurgery 
let rightArmM = armSurgeryM lowerRightArmM upperRightArmM

type DeadBrain = DeadBrain of Label 
type Skull = Skull of Label

type LiveBrain = LiveBrain of Label * VitalForce
type LiveHead = {
    brain : LiveBrain
    skull : Skull // not live
    }

let headSurgery brain skull =
    {brain=brain; skull=skull}
    
let wrapSkullInM skull = 
    let becomeAlive vitalForce = 
        skull, vitalForce 
    M becomeAlive

let makeLiveBrain (DeadBrain label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveBrain = LiveBrain (label,oneUnit)
        liveBrain, remainingVitalForce    
    M becomeAlive
    
let deadBrain = DeadBrain "Abby Normal"
let skull = Skull "Yorick"

let liveBrainM = makeLiveBrain deadBrain
let skullM = M.returnM skull

let headSurgeryM = M.map2M headSurgery
let headM = headSurgeryM liveBrainM skullM

let liveHead, remainingFromHead = runM headM vf

type DeadHeart = DeadHeart of Label 
type LiveHeart = LiveHeart of Label * VitalForce

type BeatingHeart = BeatingHeart of LiveHeart * VitalForce

let makeLiveHeart (DeadHeart label) = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let liveHeart = LiveHeart (label,oneUnit)
        liveHeart, remainingVitalForce    
    M becomeAlive
    
let makeBeatingHeart liveHeart = 
    let becomeAlive vitalForce = 
        let oneUnit, remainingVitalForce = getVitalForce vitalForce 
        let beatingHeart = BeatingHeart (liveHeart, oneUnit)
        beatingHeart, remainingVitalForce    
    M becomeAlive
    
let deadHeart = DeadHeart "Anne"
// create a live heart generator (M<LiveHeart>)
let liveHeartM = makeLiveHeart deadHeart
// create a beating heart generator (M<BeatingHeart>)
// from liveHeartM and the makeBeatingHeart function
let beatingHeartM = M.bindM makeBeatingHeart liveHeartM 
let beatingHeart, remainingFromHeart = runM beatingHeartM vf

type LiveBody = {
    leftLeg: LiveLeftLeg
    rightLeg : LiveLeftLeg
    leftArm : LiveLeftArm
    rightArm : LiveRightArm
    head : LiveHead
    heart : BeatingHeart
    }

let map3M f m1 m2 m3 =
    let becomeAlive vitalForce = 
        let v1,remainingVitalForce = runM m1 vitalForce 
        let v2,remainingVitalForce2 = runM m2 remainingVitalForce  
        let v3,remainingVitalForce3 = runM m3 remainingVitalForce2  
        let v4 = f v1 v2 v3
        v4, remainingVitalForce3    
    M becomeAlive

let (<*>) = M.applyM

let createBody leftLeg rightLeg leftArm rightArm head beatingHeart =
    {
    leftLeg = leftLeg
    rightLeg = rightLeg
    leftArm = leftArm
    rightArm = rightArm
    head = head
    heart = beatingHeart 
    }
    
let bodyM = 
    M.returnM createBody 
    <*> leftLegM
    <*> rightLegM
    <*> leftArmM
    <*> rightArmM
    <*> headM 
    <*> beatingHeartM