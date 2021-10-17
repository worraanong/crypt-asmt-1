(*
    A collection of useful functions to help solving cipher
    Note that, this is not a complete program 
*)
open System
module Cryptography =

  (* 
    Utility functions
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

  // Return number of occurrances for given letter 
  let numberOfOccurrances (letter: char) (plaintext : string) =
    Seq.filter (fun x -> x = letter) plaintext |> Seq.length 

  // Return value of a XOR b
  let XOR a b = if (a <> b) then '1' else '0'  
  
  // Convert a decimal to binary
  // 25 -> "11001"
  let rec toBinary i =
    match i with
    | 0 | 1 -> string i
    | _ -> let bit = string (i%2)
           (toBinary (i/2)) + bit

  // Convert a binary to decimal
  // "11001" -> 25
  let toNumeric binaryString = 
    let list = binaryString |> Seq.map (fun x-> (Int32.Parse <| Char.ToString x) ) |> Seq.toList
    // Not an elegant way, but this is Base 2 values
    let values = [16;8;4;2;1]
    List.zip list values |> List.sumBy (fun (a,b) -> a*b)

  // Convert a binary string to letters
  // "10000 00110 00101 00010 00100" -> QGFCE
  let binaryToLetter (binaryString: string) =
    binaryString.Split(' ')  
    |> Seq.map (toNumeric >> (fun x -> numToLetter.Item(x%26)) >> string)
    |> Seq.reduce (+)

  // Convert text to binary list
  // "DOUGH" -> ["11"; "1110"; "10100"; "110"; "111"]
  let letterToBinary (string: string) =
    string |> Seq.map((fun x -> letterToNum.Item(x)) >> toBinary) |> Seq.toList



  (* 
    Encrypt / Decryption functions
  *)

  // Shift Cipher Encryption
  // 1. Iterate throught each letter in plaintext
  // 2. Shift each letter by key 
  let shiftCipherEncrypt key plaintext =
    plaintext |> Seq.iter (fun x-> shift x key |> printf "%c")

  // Vigenere Cipher Encryption
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

  // Index Of Coincidence
  // 1. Calculate per each letter 
  //    let na = (float) (numberOfOccurrances 'A' text)
  //    let  N = (float) text.Length
  //    let Na = (na/N) * (na-1.0)/(N-1.0)
  // 2. Sum Na ... Nz
  let indexOfCoincidence (text : string) =
    let N = (float) text.Length
    let compute ni = (ni/N) * (ni-1.0)/(N-1.0)
    ['A'..'Z'] |> Seq.sumBy (fun x-> numberOfOccurrances x text |> float |> compute)

  // Affine Cipher Encrpytion
  // 1. Compute value of Y according to each letter X, using formula Y = A*X + B
  // 2. If Y > 25, Y = Y mod 26 
  let affineCipherEncrpyt (A:int) (B:int) (plaintext : string) =
    let compute (x:char) =
      let X = letterToNum.Item(x) 
      let result = A*X + B
      numToLetter.Item(result%26)
    plaintext |> Seq.iter (compute >> printf "%c")

  // Affine Cipher Decrpytion
  // 1. Compute value of X according to each letter Y, using formula Y = A(Y - B)
  // 2. If X > 25, X = X mod 26 
  // 3. If X < 0, X = X + 26 
  let affineCipherDecrpyt (A:int) (B:int) (ciphertext : string) =
    let compute (y:char) =
      let Y = letterToNum.Item(y) 
      let result = (A * (Y - B)) % 26
      let positive = if (result<0) then result+26 else result
      numToLetter.Item(positive)
    ciphertext |> Seq.iter (compute >> printf "%c")

  // OTP Encrpytion
  // 1. Prepare a binary string for plaintext and key with the same length
  // 2. XOR each bit respectively
  // 3. Note that Key and Plaintext can switch position, and Plaintext XOR Ciphertext results in Key
  // "1001101000100010010000011" "0011001011011101000111000" -> "1001101000100010010000011"
  let oneTimePadEncrypt (key: string) (plaintext : string) =
    Seq.zip key plaintext |> Seq.map ((fun (a,b) -> XOR (Int32.Parse <| Char.ToString a) (Int32.Parse <| Char.ToString b)) >> string )
            |> Seq.reduce (+)

