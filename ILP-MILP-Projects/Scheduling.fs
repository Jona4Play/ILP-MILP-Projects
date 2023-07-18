open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

[<Measure>] type Euro
[<Measure>] type Hour
[<Measure>] type Shift

type Qualification =
    | EMT = 1
    | Nurse = 2
    | Doctor = 3


//! Worker information
let workers = 
    [
        "Jenna";
        "Hannah";
        "George";
        "Freddy";
        "Kiley";
        "Delta";
        "Marlee";
        "Lawrence";
        "Tucker";
    ]

let workersQualification = 
    [
        "Jenna", Qualification.EMT
        "Hannah", Qualification.Nurse
        "George", Qualification.Doctor
        "Freddy", Qualification.Doctor
        "Kiley", Qualification.Doctor
        "Delta", Qualification.EMT
        "Marlee", Qualification.Doctor
        "Lawrence", Qualification.EMT
        "Tucker", Qualification.Nurse
    ] |> SMap.ofList

let workersWage =
    [
        "Jenna" , 25<Euro/Hour>
        "Hannah", 20<Euro/Hour>
        "George", 30<Euro/Hour>
        "Freddy", 31<Euro/Hour>
        "Kiley", 28<Euro/Hour>
        "Delta", 24<Euro/Hour>
        "Marlee", 34<Euro/Hour>
        "Lawrence", 25<Euro/Hour>
        "Tucker", 18<Euro/Hour>
    ] |> SMap.ofList

//! Shift information
let workdays = [1..7]

let shifts =
    [
        "Morning Shift"
        "Late Shift" 
        "Night Shift"
    ]

let shiftLength = 
    [
        "Morning Shift", 8<Hour/Shift>
        "Late Shift", 8<Hour/Shift>
        "Night Shift", 8<Hour/Shift>
    ] |> SMap.ofList

let shiftQualifications = 
    [
        "Morning Shift", [(1,Qualification.EMT); (1,Qualification.Doctor)]
        "Late Shift", [(1,Qualification.EMT); (1,Qualification.Doctor); (1,Qualification.Nurse)]
        "Night Shift", [(1,Qualification.Doctor)]
    ] |> SMap.ofList

//! Decision
let shouldWork =
    DecisionBuilder<Shift> "Should Work on this Day" {
        for employee in workers do
            for x in workdays do
                for shift in shifts ->
                    Boolean
    } |> SMap3.ofSeq

let employee = shouldWork.[All,1,""]

//! Constraints
let qualifiedConstraints =
    ConstraintBuilder "Qualified to work constraint" {
        for day in workdays do
            for shift in shifts ->
                let quali = shouldWork.[All,day,shift]
                let x = shiftQualifications.[shift] |> List.map snd
                for req in x do
                    for qualis in quali.Keys ->
                        ((workersQualification.[qualis]) <== req)
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
            sum(shouldWork.[employee,day, All]) <== 1.0<Shift>
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