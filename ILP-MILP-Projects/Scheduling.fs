open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

[<Measure>] type Euro
[<Measure>] type Hour
[<Measure>] type Strain
[<Measure>] type Worker


// Challenge: Create a model that is able to create schedule that minimizes costs and strain on workers while respecting these constraints:
(*
    - No worker may work over 40 hours a week
    - No worker may work 2 shifts in one day
    - Each shift requires a specific amount of workers of a certain occupation
*)

//! Domain Model
type Qualification =
    | EMT
    | Nurse
    | Doctor

type Shift = {
    Shiftname:string
    RequiredPersonal:(int * Qualification) list
    Length:float
    Strain:float
}

type Employee = {
    Name:string
    Occupation:Qualification
    Wage:float<Euro/Hour>
}

//! Worker information
let workers = 
    [
        {Name="Jenna";    Occupation = EMT;     Wage=25.0<Euro/Hour>}
        {Name="Hannah";   Occupation = Nurse;   Wage=20.0<Euro/Hour>}
        {Name="George";   Occupation = Doctor;  Wage=30.0<Euro/Hour>}
        {Name="Freddy";   Occupation = Doctor;  Wage=31.0<Euro/Hour>}
        {Name="Kiley";    Occupation = Doctor;  Wage=28.0<Euro/Hour>}
        {Name="Delta";    Occupation = EMT;     Wage=24.0<Euro/Hour>}
        {Name="Marlee";   Occupation = Doctor;  Wage=34.0<Euro/Hour>}
        {Name="Lawrence"; Occupation = EMT;     Wage=25.0<Euro/Hour>}
        {Name="Tucker";   Occupation = Nurse;   Wage=18.0<Euro/Hour>}
    ]

let workersQualification = 
    [for record in workers -> record.Name, record.Occupation] |> SMap.ofList

let workersWage =
    [for record in workers -> record.Name, record.Wage] |> SMap.ofList


//! Shift information
let workdays = [1..7]

let shifts =
    [
        {Shiftname="Morning Shift"; RequiredPersonal=[(1, EMT); (1,Doctor)];             Length=8.0;    Strain=1.2}
        {Shiftname="Late Shift";    RequiredPersonal=[(1, EMT); (1,Doctor); (1, Nurse)]; Length=8.0;    Strain=1.0}
        {Shiftname="Night Shift";   RequiredPersonal=[(1,Doctor)];                       Length=8.0;    Strain=1.8}
    ]

let shiftLength = 
    [
        "Morning Shift", 8<Hour/Shift>
        "Late Shift", 8<Hour/Shift>
        "Night Shift", 8<Hour/Shift>
    ] |> SMap.ofList


let shiftQualifications = 
    [
        (("Morning Shift", EMT), 1.0<Worker/Shift>);
        (("Morning Shift", Doctor), 1.0<Worker/Shift>);

        (("Late Shift", Nurse), 1.0<Worker/Shift>); 
        (("Late Shift", Doctor), 1.0<Worker/Shift>); 
        (("Late Shift", EMT), 1.0<Worker/Shift>);

        (("Night Shift", Doctor), 1.0<Worker/Shift>);

    ] |> SMap2.ofList


let strainOfShifts =
    [
        "Morning Shift", 1.2<Strain/Shift>
        "Late Shift", 1.0<Strain/Shift>
        "Night Shift", 2.0<Strain/Shift>
    ] |> SMap.ofList


//! Decision
let shouldWork =
    DecisionBuilder<Shift> "Should Work on this Day" {
        for employee in workers do
            for x in workdays do
                for shift in shifts ->
                    Boolean
    } |> SMap3.ofSeq



//! Constraints
let qualifiedConstraints =
    ConstraintBuilder "Is qualified and enough of in shift" {
        for employee in workers do
            for day in workdays do
                for shift in shifts do
                    for quali in occupations ->
                        shouldWork.[employee, day, shift] >== shiftQualifications.[shift, quali]
    }
let x = shouldWork.["", 1, ""]

// Maximum worktime per week
let maxHoursConstraints =
    ConstraintBuilder "Maximum Constraint" {
        for employee in workers ->
            sum (shouldWork.[employee,All,All] .* shiftLength) <== 40<Hour/Worker>
    }

// No double shift on one day can be worked
let noDoubleShiftConstraint =
    ConstraintBuilder "No Double Shift Constraint" {
        for employee in workers do
            for day in workdays ->
            sum(shouldWork.[employee,day, All]) <== 1.0<Shift>
    }

//! Objectives

let minimizeStrain =
    sum(shouldWork .* strainOfShifts)
    |> Objective.create "Minimize strain on workers" Minimize

//todo Implement a way to minimize it
//let minimizeShiftSwitch =
//    sum()
//    |> Objective.create "Minimize switches in schedule" Minimize

// Minimize costs
let objective = 
    sum(shouldWork .* shiftLength .* workersWage)
    |> Objective.create "Minimize Cost Target" Minimize


// Printing method
let printResult result =
    match result with
    | Optimal solution ->
        // printfn "Objective Value: %f" (Objective.evaluate solution objective)
        let shouldWorkHoures = Solution.getValues solution shouldWork

        printfn "Plan Cost: $%.2f" (Objective.evaluate solution objective)
        printfn "%A" shouldWorkHoures
    
        printfn "Name\tWorkweek"
        for employee in workers do
            printfn "%-12s: %12A" employee shouldWork.[employee,All,All]
    | _ -> printfn $"Unable to solve. Error: %A{result}"



objective
|> Model.create
|> Model.addConstraints qualifiedConstraints
|> Model.addConstraints noDoubleShiftConstraint
|> Model.addConstraints maxHoursConstraints
|> Solver.solve Settings.basic
|> printResult