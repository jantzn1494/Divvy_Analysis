//
// F# program to analyze Divvy daily ride data.
//
// Nathan D Jantz
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #04
//

#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [176;74;1252;21;595;1986;1]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), trip duration (secs), birth
// year (0=>not specified), and gender (0=>not specified, 
// 1=>identifies as male, 2=>identifies as female).
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

//
// GetGender
// 
// GetGender breaks down list of lists into a list of the
// seventh element of each list (gender) and stores them
// into a new list.
// 
let _getGender L = 
  match L with 
  | [] -> 0
  | _  -> L.Item(6)
  
let rec GetGender L =
  match L with
  | [] -> []
  | hd::tl -> (_getGender hd)::GetGender tl
  
//
// GetAge and AvgAge
// 
// GetAge breaks down list of lists into a list of the
// reported ages of the riders using the sixth element of
// each list (birth year) subtracted from the current year.
// 
// AvgAge checks for errors while computing average age
// 
let _getAge L = 
  match L with
  | [] -> 0.0
  | hd::tl when L.Item(5) = 0 -> 0.0
  | _  -> float (System.DateTime.Now.Year - L.Item(5))
  
let rec GetAge L =
  match L with
  | [] -> []
  | hd::tl -> (_getAge hd)::GetAge tl
  
let AvgAge L =
  match L with
  | [] -> 0.0 
  | _  -> List.average L

//
// GetTime
// 
// GetTime breaks down a list of lists into a list of the
// reported trip durations in seconds using the fifth element 
// of each list.
// 
let _getTime L =
  match L with
  | [] -> 0
  | _  -> L.Item(4)
  
let rec GetTime L =
  match L with
  | [] -> []
  | hd::tl -> (_getTime hd)::GetTime tl
  
//
// GetStart
// 
// GetStart breaks down a list of lists into a list of the
// reported trip starting hour in military time using the fourth
// element of each list.
// 
let _getStart L =
  match L with
  | [] -> 0
  | _  -> L.Item(3)
  
let rec GetStart L =
  match L with
  | [] -> []
  | hd::tl -> (_getStart hd)::GetStart tl
  
//
// printstars
// 
// printstars is a function that prints a given number of *'s
// 
let rec printstars n =
  match n with
  | 0 -> ()
  | 1 -> printf "*"
  | _ -> printf "*"
         printstars (n-1)
         
//
// Begin main
// 
[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents
  //printfn "%A" ridedata  
  
  //
  // Use ridedata to calculate # of riders
  // 
  let N = List.length ridedata
  printfn ""
  printfn "# of riders: %A" N
  printfn ""
  //
  // % identifying as male
  // 
  let GenderList = GetGender ridedata
  let maleOnlyList = List.filter (fun x -> x = 1) GenderList
  let femaleOnlyList = List.filter (fun x -> x = 2) GenderList
  
  printf "%% of riders identifying as male: %A" (List.length maleOnlyList)
  printfn " (%A%%)" ((float (List.length maleOnlyList) / float N) * float 100)
  //
  // % identifying as female
  // 
  printf "%% of riders identifying as female: %A" (List.length femaleOnlyList)
  printfn " (%A%%)" ((float (List.length femaleOnlyList) / float N) * float 100)
  printfn ""
  //
  // Average age
  // 
  let AgeList = GetAge ridedata
  let AgeOnlyList = List.filter (fun x -> x > 0.0) AgeList
  let avgAge = AvgAge AgeOnlyList
  printfn "Average age: %A" avgAge
  printfn ""
  //
  // Ride durations (0-30, 30-60, 60-120, >2) and percentages
  // 
  let DurList = GetTime ridedata
  let thirty   = List.filter (fun x -> x < 1801) DurList
  let sixty    = List.filter (fun x -> (x > 1800 && x < 3601)) DurList
  let two      = List.filter (fun x -> (x > 3600 && x < 7201)) DurList
  let aboveTwo = List.filter (fun x -> x > 7200) DurList
  
  printfn "** Ride Durations:"
  printf " 0..30 mins: %A" (List.length thirty)
  printfn " (%A%%)" ((float (List.length thirty) / float N) * float 100)
  printf " 30..60 mins: %A" (List.length sixty)
  printfn " (%A%%)" ((float (List.length sixty) / float N) * float 100)
  printf " 60..120 mins: %A" (List.length two)
  printfn " (%A%%)" ((float (List.length two) / float N) * float 100)
  printf " > 2 hours: %A" (List.length aboveTwo)
  printfn " (%A%%)" ((float (List.length aboveTwo) / float N) * float 100)
  printfn ""
  //
  // Ride start time histogram (0-23)
  // 
  let StartTimes = GetStart ridedata
  let Hour0 = List.filter(fun x -> x = 0) StartTimes
  let Hour1 = List.filter(fun x -> x = 1) StartTimes
  let Hour2 = List.filter(fun x -> x = 2) StartTimes
  let Hour3 = List.filter(fun x -> x = 3) StartTimes
  let Hour4 = List.filter(fun x -> x = 4) StartTimes
  let Hour5 = List.filter(fun x -> x = 5) StartTimes
  let Hour6 = List.filter(fun x -> x = 6) StartTimes
  let Hour7 = List.filter(fun x -> x = 7) StartTimes
  let Hour8 = List.filter(fun x -> x = 8) StartTimes
  let Hour9 = List.filter(fun x -> x = 9) StartTimes
  let Hour10 = List.filter(fun x -> x = 10) StartTimes
  let Hour11 = List.filter(fun x -> x = 11) StartTimes
  let Hour12 = List.filter(fun x -> x = 12) StartTimes
  let Hour13 = List.filter(fun x -> x = 13) StartTimes
  let Hour14 = List.filter(fun x -> x = 14) StartTimes
  let Hour15 = List.filter(fun x -> x = 15) StartTimes
  let Hour16 = List.filter(fun x -> x = 16) StartTimes
  let Hour17 = List.filter(fun x -> x = 17) StartTimes
  let Hour18 = List.filter(fun x -> x = 18) StartTimes
  let Hour19 = List.filter(fun x -> x = 19) StartTimes
  let Hour20 = List.filter(fun x -> x = 20) StartTimes
  let Hour21 = List.filter(fun x -> x = 21) StartTimes
  let Hour22 = List.filter(fun x -> x = 22) StartTimes
  let Hour23 = List.filter(fun x -> x = 23) StartTimes

  printfn "** Ride Start Time Histogram:"
  printf " 0: "
  printstars ((List.length Hour0) / 10)
  printfn "%A" (List.length Hour0)
  printf " 1: "
  printstars ((List.length Hour1) / 10)
  printfn "%A" (List.length Hour1)
  printf " 2: "
  printstars ((List.length Hour2) / 10)
  printfn "%A" (List.length Hour2)
  printf " 3: "
  printstars ((List.length Hour3) / 10)
  printfn "%A" (List.length Hour3)
  printf " 4: "
  printstars ((List.length Hour4) / 10)
  printfn "%A" (List.length Hour4)
  printf " 5: "
  printstars ((List.length Hour5) / 10)
  printfn "%A" (List.length Hour5)
  printf " 6: "
  printstars ((List.length Hour6) / 10)
  printfn "%A" (List.length Hour6)
  printf " 7: "
  printstars ((List.length Hour7) / 10)
  printfn "%A" (List.length Hour7)
  printf " 8: "
  printstars ((List.length Hour8) / 10)
  printfn "%A" (List.length Hour8)
  printf " 9: "
  printstars ((List.length Hour9) / 10)
  printfn "%A" (List.length Hour9)
  printf " 10: "
  printstars ((List.length Hour10) / 10)
  printfn "%A" (List.length Hour10)
  printf " 11: "
  printstars ((List.length Hour11) / 10)
  printfn "%A" (List.length Hour11)
  printf " 12: "
  printstars ((List.length Hour12) / 10)
  printfn "%A" (List.length Hour12)
  printf " 13: "
  printstars ((List.length Hour13) / 10)
  printfn "%A" (List.length Hour13)
  printf " 14: "
  printstars ((List.length Hour14) / 10)
  printfn "%A" (List.length Hour14)
  printf " 15: "
  printstars ((List.length Hour15) / 10)
  printfn "%A" (List.length Hour15)
  printf " 16: "
  printstars ((List.length Hour16) / 10)
  printfn "%A" (List.length Hour16)
  printf " 17: "
  printstars ((List.length Hour17) / 10)
  printfn "%A" (List.length Hour17)
  printf " 18: "
  printstars ((List.length Hour18) / 10)
  printfn "%A" (List.length Hour18)
  printf " 19: "
  printstars ((List.length Hour19) / 10)
  printfn "%A" (List.length Hour19)
  printf " 20: "
  printstars ((List.length Hour20) / 10)
  printfn "%A" (List.length Hour20)
  printf " 21: "
  printstars ((List.length Hour21) / 10)
  printfn "%A" (List.length Hour21)
  printf " 22: "
  printstars ((List.length Hour22) / 10)
  printfn "%A" (List.length Hour22)
  printf " 23: "
  printstars ((List.length Hour23) / 10)
  printfn "%A" (List.length Hour23)

  0 
