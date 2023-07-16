open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

[<Measure>] type Euro
[<Measure>] type Hour

type Qualification =
    | EMT
    | Nurse
    | Doctor


//! Worker information
let workers = 
    [
        "Jenna";
        "Hannah";
        "George"
    ]

let workersQualification = 
    [
        "Jenna", EMT
        "Hannah", Nurse
        "George", Doctor
    ] |> SMap.ofList

let workersWage =
    [
        "Jenna" , 25<Euro/Hour>
        "Hannah", 20<Euro/Hour>
        "George", 30<Euro/Hour>
    ] |> SMap.ofList

//! Shift information
let workdays = [1..7]

let shifts =
    [
        "Morning Shift";
        "Late Shift";
        "Night Shift"
    ]

let shiftLength = 
    [
        "Morning Shift", 8<Hour>
        "Late Shift", 8<Hour>
        "Night Shift", 8<Hour>
    ] |> SMap.ofList

let shiftQualifications = 
    [
        "Morning Shift", [(1,EMT); (1,Doctor)]
        "Late Shift", [(1,EMT); (1,Doctor); (1,Nurse)]
        "Night Shift", [(1,Doctor)]
    ] |> SMap.ofList

//! Decision
let shouldWork =
    DecisionBuilder<Hour> "Should Work on this Day" {
        for employee in workers do
            for x in workdays do
                for shift in shifts ->
                    Boolean
    } |> SMap3.ofSeq


//! Constraints
let qualifiedConstraints =
    ConstraintBuilder "Qualified to work constraint" {
        
    }

let minimalStaffingConstraints =
    ConstraintBuilder "Minimal amount of qualified workers" {

    }

// Maximum worktime per week
let maxHoursConstraints =
    ConstraintBuilder "Maximum Constraint" {
        for employee in workers ->
            sum (shouldWork.[employee,All,All] .* shiftLength) <== 40<Hour>
    }

// No double shift on one day can be worked
let noDoubleShiftConstraint =
    ConstraintBuilder "No Double Shift Constraint" {
        for employee in workers do
            for day in workdays ->
            sum(shouldWork.[employee,day, All]) <== 1.0<Hour>
    }


//! Minimize costs
let objective = 
    sum(shouldWork .* workersWage .* shiftLength)
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
|> Model.addConstraints minimalStaffingConstraints
|> Model.addConstraints noDoubleShiftConstraint
|> Model.addConstraints maxHoursConstraints
|> Solver.solve Settings.basic
|> printResult