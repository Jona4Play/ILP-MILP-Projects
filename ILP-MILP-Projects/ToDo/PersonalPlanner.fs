module PersonalPlanner

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

[<Measure>] type Euro
[<Measure>] type Hour
[<Measure>] type Strain
[<Measure>] type Worker
[<Measure>] type Shift


// Challenge: Create a list of ideal employees from a schedule that minimizes 
// costs while respecting these constraints:
(*
    - No worker may work over 40 hours a week
    - No worker may work 2 shifts in one day
    - Each shift requires a specific amount of workers of a certain occupation
*)
// As well as minimizes possible code duplications and maximizes extensibility and modularity


//! Domain Model
type Qualification =
    | EMT
    | Nurse
    | Doctor

let qualifications = [EMT; Nurse; Doctor]

type ShiftInfo = {
    Name:string
    RequiredPersonal:SMap<Qualification,int<Worker/Shift>>
    Length:float<Hour/Shift>
}

type Employee = {
    Occupation:Qualification
    Wage:float<Euro/Hour>
}


let payPerQualification =
    [
        Nurse, 19.0<Euro/Hour>
        EMT, 25.0<Euro/Hour>
        Doctor, 30.0<Euro/Hour>
    ] |> SMap.ofList

//! Shift information
let shifts =
    [
        {Name="Morning Shift"; RequiredPersonal=SMap[(Doctor, 1<Worker/Shift>); (EMT, 1<Worker/Shift>); (Nurse, 0<Worker/Shift>)]; Length=8.0<Hour/Shift>;}
        {Name="Late Shift";    RequiredPersonal=SMap[(Doctor, 1<Worker/Shift>); (EMT, 1<Worker/Shift>); (Nurse, 1<Worker/Shift>)]; Length=8.0<Hour/Shift>;}
        {Name="Night Shift";   RequiredPersonal=SMap[(Doctor, 1<Worker/Shift>); (EMT, 0<Worker/Shift>); (Nurse, 0<Worker/Shift>)]; Length=8.0<Hour/Shift>;}
    ]

//! Shift information
let workdays = [1..7]

let schedule =
    [
        for day in workdays ->
            day,shifts
    ] |> SMap.ofList





//! Decision
let numberOfWorkersByProfession =
    DecisionBuilder<Worker> "How many workers for a given weekly schedule" {
        for qualification in qualifications ->
            Integer(0.0<Worker>, 1_000_000.0<Worker>)
    } |> SMap.ofSeq


//! Constraints
(*
    We need more or an equal amount of workers of the matching profession to be working per shift requirements:
    - shouldWork.[Where(employee = reqProfession), day, shift] >== Count<Worker/Shift>
    
    Each worker can only work a certain amount of hours
    - shouldWork.[employee, All, All] <== x<Hour>

    No worker can enter 2 shifts per day
    - shouldWork.[employee, day, All] <== 1.0<Shift>
*)

// Maximum worktime per week
let minWorkerRequirement =
    ConstraintBuilder "Minimal amount of workers" {
        for day in workdays do
            let shiftsPerDay = schedule.[day]
            for shift in shiftsPerDay do
                for qualification in qualifications ->
                    numberOfWorkersByProfession.[qualification] >== float(shift.RequiredPersonal.[qualification]) * 1.0<Worker>
    }


// No double shift on one day can be worked
let noDoubleShiftConstraint =
    ConstraintBuilder "No Double Shift Constraint" {
        for day in workdays do
            let shiftsPerDay = schedule.[day]
            for qualification in qualifications ->
                numberOfWorkersByProfession.[qualification] >== float([for shift in shiftsPerDay do shift.RequiredPersonal.[qualification]] |> List.sum) * 1.0<Worker>
    }



let minimizeCosts = 
    let payPerQualification = numberOfWorkersByProfession .* payPerQualification
    [for qualification in qualifications -> payPerQualification.[qualification]] |> List.sum
    |> Objective.create "Minimize Cost Target" Minimize

// Printing method
let printResult result =
    match result with
    | Optimal solution ->
        printfn "Minimal personal costs:      %.2f" (Objective.evaluate solution minimizeCosts)
        
        let values = Solution.getValues solution numberOfWorkersByProfession

        for qualification in qualifications do
            printfn "#%s: %f" (qualification.ToString()) (values.[qualification])

    | _ -> printfn $"Unable to solve. Error: %A{result}. This might be because of a problem in the domain model or a conflicting constraint"


//! Solve the model
let solve () =
    minimizeCosts
    |> Model.create
    |> Model.addConstraints minWorkerRequirement
    |> Model.addConstraints noDoubleShiftConstraint
    |> Solver.solve Settings.basic
    |> printResult