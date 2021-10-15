(*
    A collection of useful functions to help solving cipher
    Note that, this is not a complete program 
*)

// Create a dictionary of Alphabet and its corresponding numeric value
let letterToNum = List.zip ['A'..'Z'] [0..25] |> dict
let numToLetter = List.zip [0..25] ['A'..'Z'] |> dict

// Return a letter after shift z position; numeric value as key
let shift (x : char) (z : int) =
  let shifted = letterToNum.Item(x) + z
  numToLetter.Item(shifted % 26) // n = 26 letters 

// Return a letter after shift by y; another letter as key
let shiftBy (x : char) (y : char) =
  let shifted = letterToNum.Item(x) + letterToNum.Item(y)
  numToLetter.Item(shifted % 26) // n = 26 letters 

// Shift Cipher
// 1. Iterate throught each letter in plaintext
// 2. Shift each letter by key 
let shiftCipherEncrypt key plaintext =
  plaintext |> Seq.iter (fun x-> shift x key |> printf "%c")

// VigenereCipherEncrypt
// 1. Repeat a key until the length is longer or equal to plaintext
// 2. Zip them together as keyPair in form of [(p0, k0); (p1, k1); ... (pn, kn);]
// 3. Iterate throught each pair
// 4. Shift each letter by its own key 
let vigenereCipherEncrypt (key: string) (plaintext : string) =
  let repeatedTime = plaintext.Length / key.Length + 1
  let keys = Seq.toList key
  let repeatedKeys = List.replicate repeatedTime keys |> List.concat
  let keyPair = Seq.zip plaintext repeatedKeys
  keyPair |> Seq.iter (fun (x,y)-> shiftBy x y|> printf "%c")

let numberOfOccurrances (letter: char) (plaintext : string) =
  Seq.filter (fun x -> x = letter) plaintext |> Seq.length 

(*IndexOfCoincidence
  1. Calculate per each letter 
     let na = (float) (numberOfOccurrances 'A' text)
     let  N = (float) text.Length
     let Na = (na/N) * (na-1.0)/(N-1.0)
  2. Sum Na ... Nz
*)
let indexOfCoincidence (text : string) =
  let N = (float) text.Length
  let compute ni = (ni/N) * (ni-1.0)/(N-1.0)
  ['A'..'Z'] |> Seq.sumBy (fun x-> numberOfOccurrances x text |> float |> compute)
  

// Assignment tasks

(**********
   Task 1
***********) 
let plaintext = "BLOCKCHAIN"
let ks1 = 9
shiftCipherEncrypt ks1 plaintext
// S1 = KUXLTLQJRW

// P1 is done by hand

let P1 = "ULXTKQRJWL"
let ks2 = 19
shiftCipherEncrypt ks2 P1
// S2 = NEQMDJKCPE

// P2 is done by hand

(**********
   Task 2
***********) 
let plaintextT2 = "FRIENDSMAKETHEWORSTENEMIES"
let keyT2 = "LIST"
vigenereCipherEncrypt keyT2 plaintextT2
// QZAXYLKFLSWMSMOHCALXYMEBPA

indexOfCoincidence plaintextT2
// 0.07076923077

indexOfCoincidence "QZAXYLKFLSWMSMOHCALXYMEBPA"
// 0.03692307692

