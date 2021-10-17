// Assignment 

open Cryptography

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


(**********
   Task 3
***********) 
let plaintextT3  = "SURFACE"
let ciphertextT3 = "NJCAXTP"

let numP = plaintextT3 |> Seq.map (fun x-> letterToNum.Item(x))
numP |> Seq.iter (printf "%A ")
// 18 20 17 5 0 2 4 
let numC = ciphertextT3 |> Seq.map (fun x-> letterToNum.Item(x))
numC |> Seq.iter (printf "%A ")
// 13 9 2 0 23 19 15
let differences = Seq.zip numC numP |> Seq.map (fun (x,y) -> x - y)
differences |> Seq.iter (printf "%A ")
// -5 -11 -15 -5 23 17 11

// Find Encryption key 
// 'A' -> 'X'
// AX + B = 23
// 0 * X + B = 23
// B = 23 
// 'C' -> 'T'
// A * 2 + 23 = 19
// A = -4/2
// A = -2
// But we don't want negative X value; add 26
// A * 2 + 23 = 19 + 26
// A = 22/2
// A = 11

// 11X + 23
// Verify
affineCipherEncrpyt 11 23 plaintextT3 
// NJCAXTP

// Find Decryption key
// 1 = (11 * A) % 26 
// A = 45
// 'X' -> 'A'
// 45(23 - B) = 0
// B = 23

// 45(Y-23)
// Verify
affineCipherDecrpyt 45 23 ciphertextT3
// SURFACE


(**********
   Task 4
***********) 
let m1 = "DOUGH"
let binaryM1 = letterToBinary m1
//  ["11"; "1110"; "10100"; "110"; "111"]

let fm1 = "0001101110101000011000111"
let c1  = "1000000110001010001000100"

// Find the key

let key = oneTimePadEncrypt fm1 c1
// "1001101000100010010000011"

let m2 = "GLORY"
let binaryM2 = letterToBinary m2
// ["110"; "1011"; "1110"; "10001"; "11000"]

let fm2 = "0011001011011101000111000"
let c2 = oneTimePadEncrypt key fm2
// "1010100011111111010111011"

let ciphertextT4 = binaryToLetter "10101 00011 11111 10101 11011"
// "VDFVB"