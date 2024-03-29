open System //To use Math.PI



// 1.Given a list of integers, return a new list with each element squared.
let squareList (inputList: int list) =
    List.map (fun x -> x * x) inputList

// Test:
let myList = [5; 2; 8; 6; 9; -2]
let squaredList = squareList myList
printfn "1. %A" squaredList



// 2. Write a function that filters out all odd numbers from a given list of integers, returning a list of only the even numbers.
let evenList (inputList: int list) =
    List.filter(fun x -> x % 2 = 0) inputList

// Test:
let evenExampleList = evenList myList
printfn "2. %A" evenExampleList



// 3.Write a function that takes a list of numbers and returns the sum of all positive numbers in the list.
let sumOfPositive (inputList: int list) =
    inputList |> List.filter(fun x -> x > 0) |> List.sum 

// Test:
let sumOfPositiveNum = sumOfPositive myList
printfn "3. %A" sumOfPositiveNum



// 4. Given a list of names (strings), return a new list with each name capitalized.
let capitaliseList (inputList: string list) =
    List.map(fun x -> x.ToString().ToUpper()) inputList

// Test:
let stringList = ["abc"; "12aijid"; "hello"; "oksjkalsj55"; "12aijid";]
let capitalisedList = capitaliseList stringList 
printfn "4. %A" capitalisedList



// 5. Write a function that takes a list of strings and an integer n, returning a list of strings where each string has a length greater than n. 
let charCount (inputList, num) = 
    List.filter(fun x -> String.length x <= num) inputList

// Test:
let charCounted = charCount (stringList, 5)
printfn "5. %A" charCounted



// 6. Write a function that takes a list of integers and an divisor, and returns the count of numbers which are divisible by it.
let moduloFilter (inputList, num) = 
    inputList |> List.filter(fun x -> x%num = 0) |> List.length

// Test:
let moduloFiltered = moduloFilter (myList, 3)
printfn "6. %i" moduloFiltered



//7. Create a function that takes a list and an element, and returns all the indices of the list where this element can be found.
let checkElementExistance (inputList: 'a list, el: 'a) = 
   inputList 
   |> List.mapi(fun index value -> if value.ToString().Contains(el.ToString()) then index.ToString() else "NaM" ) 
   |> List.filter(fun x -> x<>"NaM")

// Test:
let indexes = checkElementExistance(stringList, "ai")
printfn "7. %A" indexes



// 8. Given a list of strings, write a function that concatenates only those strings that are longer than a given length n.
let concatenate (inputList: string list, num: int) =
    inputList |> List.filter(fun x -> x.ToString().Length > 4) |> String.concat ""

// Test:
let concatenatedString = concatenate (stringList, 4)
printfn "8. %s" concatenatedString



// 9. Assuming a list of tuples where each tuple contains an (id, value), write a function to find the tuple with the maximum value.
let tupleList = [(0, 10); (1, 20); (2, 5); (3, 10); (4, 22)]

let tupleMaxFinder (tuples: ('id * 'value) list) = 
    tuples |> List.maxBy snd 

// Test:
let tupleMax = tupleMaxFinder tupleList
printfn "9. %A" tupleMax



// 10. Given a list of elements that could repeat, write a function that returns a list of tuples, each containing an element from the input list and the number of times it appears.
let frequencyConunter (inputList: 'a list) = 
    inputList |> List.groupBy id |> List.map (fun (value, occurence) -> (value, List.length occurence))

// Test:
let testFreqCounter = frequencyConunter stringList
printfn "10. %A" testFreqCounter



// 11. Define a DU for a traffic light (Red, Yellow, Green). Write a function that takes a traffic light state and returns the next state.

type TrafficLights = Red | Yellow | Green

let nextLight (currentLight : TrafficLights) =
    match currentLight with
    | Red -> Green
    | Yellow -> Red
    | Green -> Yellow

// Test:
let getNextLight = nextLight Yellow
printfn "11. %A" getNextLight



// 12. Create a DU for basic arithmetic operations (Add, Subtract, Multiply, Divide). Implement a function that takes two numbers and an operation, then performs the operation on the numbers.
type Operations = Add | Subtract | Multiply | Divide

let mathOperation (x: float, y: float, operation: Operations) =
    match operation with
    | Add -> x + y
    | Subtract -> x - y
    | Multiply -> x * y
    | Divide -> 
        if y = 0.0 then
            failwith "Division by zero is not allowed"
        else
            x / y

// Test:
let mathOperate = mathOperation (1, 2, Add)
printfn "12. %f" mathOperate



// 13. Define a DU for different shapes (e.g., Circle, Rectangle, Square, etc.) Write a function that calculates the area of the given shape.
type Shapes = Circle of float | Rectangle of float * float | Square of float | Triangle of float * float

let areaCalculator (shape:Shapes) =
    match shape with
    | Circle(r) -> Math.PI * r * r
    | Rectangle(a, b) -> a * b
    | Square(a) -> a * a
    | Triangle(h, l) -> 0.5 * h * l

// Test:
let area = areaCalculator (Rectangle (5.0, 5.0))
printfn "13. %f" area



// 14. Define a DU for temperature scales (Celsius, Fahrenheit). Write a function that converts temperatures between the scales.
type Temp = Celcius of float | Fahrenheit of float

let tempConvertor (temp: Temp) =
    match temp with
    | Celcius(x) -> (x * 1.8) + 32.0
    | Fahrenheit(x) -> (x - 32.0) * (5.0 / 9.0)

// Test: 
let testTempConvertor = tempConvertor (Fahrenheit 62)
printfn "14. %f" testTempConvertor



// 15. Create a DU to represent a simplified JSON value. Include cases for JsonObject, JsonArray, JsonString, JsonNumber, and JsonBoolean. Write a function that takes such a JSON value and pretty-prints it to a string.
type JsonValue =
    | JsonObject of (string * JsonValue) list
    | JsonArray of JsonValue list
    | JsonString of string
    | JsonNumber of float
    | JsonBoolean of bool

let rec prettyPrintJsonValue (jsonValue : JsonValue) =
    match jsonValue with
    | JsonObject(entries) ->
        let entriesStr =
            entries
            |> List.map (fun (key, value) -> "\"" + key + "\": " + prettyPrintJsonValue value)
            |> String.concat ", "
        "{ " + entriesStr + " }"
    | JsonArray(elements) ->
        let elementsStr =
            elements
            |> List.map prettyPrintJsonValue
            |> String.concat ", "
        "[ " + elementsStr + " ]"
    | JsonString(str) -> "\"" + str + "\""
    | JsonNumber(num) -> string num
    | JsonBoolean(true) -> "true"
    | JsonBoolean(false) -> "false"

// Test:
let testJson =
    JsonObject
        [ "name", JsonString "John"
          "age", JsonNumber 30.0
          "isStudent", JsonBoolean true
          "courses", JsonArray [JsonString "Math"; JsonString "Physics"]
          "address",
              JsonObject
                  [ "street", JsonString "123 Main St"
                    "city", JsonString "Anytown"
                    "zip", JsonString "12345" ] ]

let jsonString = prettyPrintJsonValue testJson
printfn "15. %s" jsonString



// 16. Write a recursive function to compute the nth Fibonacci number.
let rec getFibonacci n = 
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> getFibonacci (n - 1) + getFibonacci(n - 2)

let testGetFibonacci = getFibonacci 5
printfn "16. %d" testGetFibonacci



// 17. Implement a recursive binary search algorithm that searches for a given element within a sorted array.
let rec binarySearch (arr : int[]) (target : int) (left : int) (right : int) =
    if left > right then
        None
    else
        let mid = (left + right) / 2
        match arr.[mid] with
        | midValue when midValue = target -> Some mid
        | midValue when midValue > target -> binarySearch arr target left (mid - 1)
        | _ -> binarySearch arr target (mid + 1) right

let binarySearchMain (arr : int[]) (target : int) =
    binarySearch arr target 0 (Array.length arr - 1)

// Test:
let arr = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 9; 10;|]
let target = 9
match binarySearchMain arr target with
| Some index -> index, printfn "17. Index of El: %d" index
| None -> -1, printfn "17. Element not found" 



// 18. Write a recursive function to sort a list of integers using the merge sort algorithm.
let rec mergeSort (inputList : int list) =
    let rec merge (left : int list) (right : int list) =
        match left, right with
        | [], _ -> right
        | _, [] -> left
        | x::xs, y::ys ->
            if x <= y then
                x :: merge xs (y::ys)
            else
                y :: merge (x::xs) ys
    
    let rec split (inputList : int list) =
        let rec splitAux (inputList : int list) (left : int list) (right : int list) =
            match inputList with
            | [] -> (left, right)
            | x::xs -> splitAux xs right (x::left)
        let half = List.length inputList / 2
        splitAux (List.take half inputList) [] (List.skip half inputList)
    
    match inputList with
    | [] | [_] -> inputList
    | _ ->
        let left, right = split inputList
        merge (mergeSort left) (mergeSort right)

// Test usage:
let unsortedList = [4; 2; 7; 1; 9; 5; 3; 8]
let sortedList = mergeSort unsortedList
printfn "18. %A" sortedList



// 19. Given a binary tree (as a nested structure of nodes), write a recursive function to compute the depth of the tree.
type BinaryTree<'a> =
    | Empty
    | Node of 'a * BinaryTree<'a> * BinaryTree<'a>

let rec treeDepth<'a> (tree : BinaryTree<'a>) =
    match tree with
    | Empty -> 0
    | Node(_, left, right) -> 1 + max (treeDepth left) (treeDepth right)

// Test:
let tree =
    Node(1,
        Node(2,
            Node(4, Empty, Empty),
            Empty),
        Node(3, 
            Node(5, 
                Node(6, 
                    Node(7, Empty, Empty), 
                    Empty), 
                Empty),
            Empty))

// Test:
let testTreeDepth = treeDepth tree
printfn "19. %d" testTreeDepth



// 20. Create a recursive function to check whether a given string is a palindrome (reads the same backward as forward).
let rec checkPalindrome (s: string) =
    match s.Length with
    | 0|1 -> true
    | _ -> if s[0] = s[s.Length - 1] then
                checkPalindrome (s.Substring(1, s.Length - 2))
            else 
                false

// Test:
let testCheckPalindrome = checkPalindrome "palindrome"
printfn "20. %b" testCheckPalindrome


// To keep console window open
System.Console.ReadLine()