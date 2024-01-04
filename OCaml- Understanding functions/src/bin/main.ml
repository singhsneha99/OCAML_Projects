
(* file: main.ml
   
  Problem set 0: exercises for coming up to speed in OCaml.

  Usage from parent directory:

   > dune exec bin/main.exe test

*)

(* Problem 1: 

   isDigit : char -> bool
*)
let isDigit (d:char) : bool =
  match d with
  |'0'..'9'->true
  |_->false;;
(* Problem 2: 

   newFileName : string -> string -> string
*)
let newFileName oldFileName newExtension =
  match String.rindex_opt oldFileName '.' with
  | None -> oldFileName
  | Some ri -> 
      let name = String.sub oldFileName 0 ri in
      Format.sprintf "%s.%s" name newExtension;;

(* Problem 3: 

   homePath : unit -> string list
*)
let homePath()=
let s=(Sys.getcwd()) in
String.split_on_char '/' s;;

(* Problem 4: 
   atoi : string -> int
*)
let atoi w=
int_of_string w;;

type token = If | And | Or


(* Problem 5: 

   tokenize : string -> token list
*)
let tokenize s= 
let mm=List.filter (fun x->x <> "") (String.split_on_char ' ' s)in
let update ss=
match ss with
|"if"-> If
|"||"->Or
|"&&"->And 
|_->If
in
List.map (fun x-> update x) mm;;

(*let tokenize _ =
  failwith "Problem 5: tokenize not yet implemented."*)

(* Two problems related to trees. We have arrows as interior nodes
   and leaves that include a unique constant C and variables
   {v1, v2, ... }

               ->             ->
              /  \           /  \
            v0   ->         C    v1
                /  \
               C    C
 *)

(* Note that the 'too' field should probably be called 'to' (that is the names would be from and to), but 'to'
   is already a reserved keyword in OCaml, so we cannot use it for a field name. *)

type t = C
       | Var of int
       | Arrow of { from : t
                  ; too  : t
                  }

(* Problem 6: 

   formatTree : t -> string
*)
(*let formatTree _ =
  failwith "Problem 6: formatTree not yet implemented."*)

  let rec formatTree tree =
    match tree with
    | C -> "C"
    | Var n -> "v" ^ string_of_int n
    | Arrow { from; too } ->
      "(" ^ formatTree from ^ " -> " ^ formatTree too ^ ")"

(* Problem 7: 

   subplaindrome : string -> string
*)
let string_rev str =
  let rec aux  idx = match idx with
    0 -> Char.escaped (str.[0])
  | _ -> (Char.escaped str.[idx]) ^ (aux (idx-1)) in
 aux ((String.length str)-1) ;;
 let rec subpalindrome s=
 if (s="") then s else
 if (((String.length s)>0) && (s=(string_rev s))) then
 s else
  subpalindrome (String.sub s 1 ((String.length s)-2));;


(* Problem 8: 
  
   list_comparisons : int list -> comparison list
*)
type comparison = GEQ | LT

let listComparisons nums =
  match nums with
  | [] | [_] -> []
  | n :: rest ->
    let rec loop prev_acc nums =
      match nums with
      | [] -> List.rev prev_acc
      | m :: rest_nums ->
        let comp = if n <= m then GEQ else LT in
        loop (comp :: prev_acc) rest_nums
    in
    GEQ :: loop [] rest
        


(* Problem 9: 

   patience : (int list) list -> int -> (int list) list

let patience _ _ = failwith "Problem 9: patience not implemented"*)

let rec patience piles n =
  match piles with
  | [] -> [[n]]
  | pile :: rest ->
    if n <= List.hd pile then (n :: pile) :: rest
    else pile :: patience rest n

(* Problem 10 : 

module type FilteredSetType = sig
  type t
  val newSet : (int -> bool) -> t
  val insert : int -> t -> t
  val member : int -> t -> bool
  val mapAndFilter : (int -> int) -> t -> t
end

module FilteredSet : FilteredSetType =
struct
  type t = unit
  let newSet _ = failwith "Problem 4: newSet not implemented"
  let insert _ _ = failwith "Problem 4: insert not implemented"
  let member _ _ = failwith "Problem 4: member not implemented"
  let mapAndFilter _ _ = failwith "Problem 4: mapAndFilter not implemented"
end*)

module type FilteredSetType = sig
  type t
  val newSet : (int -> bool) -> t
  val insert : int -> t -> t
  val member : int -> t -> bool
  val mapAndFilter : (int -> int) -> t -> t
end

module FilteredSet : FilteredSetType =
struct
  type t = { filter: int -> bool; set: int list }

  let newSet f = { filter = f; set = [] }

  let insert n s =
    if s.filter n then
      { s with set = n :: s.set }
    else
      s

  let member n s = List.mem n s.set

  let mapAndFilter g s =
    let filtered_set = List.filter s.filter s.set in
    let mapped_filtered_set = List.map g filtered_set in
    { s with set = mapped_filtered_set }
end



(* TESTING **********************************************************)

type parts = One       (* isDigit *)
           | Two       (* newFileName *)
           | Three     (* homePath *)
           | Four      (* atoi *)
           | Five      (* tokenize *)
           | Six       (* formatTree *)
           | Seven       (* subpalindrome *)
           | Eight       (* listComparisons *)
           | Nine       (* patience *)
           | Ten       (* FilteredSet module *)

(* Some simple test data
*)
let v0 = Lib.fresh()
let v1 = Lib.fresh()
let v2 = Lib.fresh()

(*            t0 = ->        t1 = ->
                  /  \           /  \
                v0   ->         C    v1
                    /  \
                   C    C
*)
let t0 = Arrow { from = Var v0
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }

let t1 = Arrow { from = C
               ; too  = Var v1
               }

let t2 = Arrow { from = t1
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }
let t3 = Arrow { from = C
               ; too  = t0
               }

(********************************************************************)

(* Test isDigit
*)
let isDigitTest1 () = isDigit '0'
let isDigitTest2 () = isDigit '9'
let isDigitTest3 () = not (isDigit 'A')
let isDigitTests () =
  Lib.run_test "isDigit test1" isDigitTest1 ;
  Lib.run_test "isDigit test2" isDigitTest2 ;
  Lib.run_test "isDigit test3" isDigitTest3

(* Test newFileName
*)
let newFileNameTest1 () = newFileName "A.java" "class" = "A.class"
let newFileNameTest2 () = newFileName "A" "class" = "A"
let newFileNameTest3 () = newFileName "A.B.java" "class" = "A.B.class"
let newFileNameTest4 () = newFileName "A." "class" = "A.class"
let newFileNameTests () =
  Lib.run_test "newFileName test1" newFileNameTest1 ;
  Lib.run_test "newFileName test2" newFileNameTest2 ;
  Lib.run_test "newFileName test3" newFileNameTest3 ;
  Lib.run_test "newFileName test4" newFileNameTest4

(* Test homePath
*)
let homePathTest () =
  let answer = homePath () in
  let combiner name1 name2 = Lib.fmt "%s/%s" name1 name2 in
  let path = List.fold_left combiner "" answer
  in
  try
    Unix.chdir path = ()
  with
    Unix.Unix_error _ -> failwith "homeTest failed"
let homePathTests () = Lib.run_test "home test" homePathTest

(* Test atoi
*)
let atoiTest1 () = atoi "345" = 345
let atoiTest2 () = atoi "0" = 0
let atoiTests () =
  Lib.run_test "atoi test1" atoiTest1 ;
  Lib.run_test "atoi test2" atoiTest2

(* Test tokenize
*)
let tokenizeTest1 () =
  (tokenize "|| if && if && ") = [Or; If; And; If; And]
let tokenizeTest2 () = (tokenize "||") = [Or]
let tokenizeTest3 () = (tokenize "       ") = []
let tokenizeTests () =
  Lib.run_test "tokenize test1" tokenizeTest1 ;
  Lib.run_test "tokenize test2" tokenizeTest2 ;
  Lib.run_test "tokenize test3" tokenizeTest3

(* Test formatTree
*)
let formatTreeTest1 () = (formatTree t0) = "(v0 -> (C -> C))"
let formatTreeTest2 () = (formatTree t1) = "(C -> v1)"
let formatTreeTest3 () = (formatTree (Var v2)) = "v2"
let formatTreeTests () =
  Lib.run_test "formatTree test1" formatTreeTest1 ;
  Lib.run_test "formatTree test2" formatTreeTest2 ;
  Lib.run_test "formatTree test3" formatTreeTest3

(* Test subpalindrome
*)
let subpalindromeTest1 () = subpalindrome "aba" = "aba"
let subpalindromeTest2 () = subpalindrome "dabac" = "aba"
let subpalindromeTest3 () = subpalindrome "xx" = "xx"
let subpalindromeTest4 () = subpalindrome "x1amanaplanacanalpanamax1" = "amanaplanacanalpanama"
let subpalindromeTest5 () = subpalindrome "civic" = "civic"
let subpalindromeTest6 () = subpalindrome "deified" = "deified"
let subpalindromeTest7 () = subpalindrome "2eifie+" = "eifie"
let subpalindromeTest8 () = subpalindrome "xyz" = "y"
let subpalindromeTest9 () = subpalindrome "" = ""
let subpalindromeTests () =
  Lib.run_test "subpalindrome test1" subpalindromeTest1 ;
  Lib.run_test "subpalindrome test2" subpalindromeTest2 ;
  Lib.run_test "subpalindrome test3" subpalindromeTest3 ;
  Lib.run_test "subpalindrome test4" subpalindromeTest4 ;
  Lib.run_test "subpalindrome test5" subpalindromeTest5 ;
  Lib.run_test "subpalindrome test6" subpalindromeTest6 ;
  Lib.run_test "subpalindrome test7" subpalindromeTest7 ;
  Lib.run_test "subpalindrome test8" subpalindromeTest8 ;
  Lib.run_test "subpalindrome test9" subpalindromeTest9

let listComparisonsTest1 () = listComparisons [3] = [GEQ]
let listComparisonsTest2 () = listComparisons [3;4;5] = [GEQ; GEQ; GEQ]
let listComparisonsTest3 () = listComparisons [1;-1;1] = [GEQ; LT; GEQ]
let listComparisonsTest4 () = listComparisons [-1;-1;1] = [GEQ; GEQ; GEQ]
let listComparisonsTest5 () = listComparisons [9;8;7] = [GEQ; LT; LT]
let listComparisonsTest6 () = listComparisons [9;8;7;10] = [GEQ; LT; LT; GEQ]
let listComparisonsTests () =
  Lib.run_test "listComparisons test1" listComparisonsTest1 ;
  Lib.run_test "listComparisons test2" listComparisonsTest2 ;
  Lib.run_test "listComparisons test3" listComparisonsTest3 ;
  Lib.run_test "listComparisons test4" listComparisonsTest4 ;
  Lib.run_test "listComparisons test5" listComparisonsTest5 ;
  Lib.run_test "listComparisons test6" listComparisonsTest6 

let patienceTest1 () = patience [[3]] 4 = [[3]; [4]]
let patienceTest2 () = patience [] 3 = [[3]]
let patienceTest3 () = patience [[4]; [5]] 3 = [[3;4]; [5]]
let patienceTest4 () = patience [[2]; [6]] 4 = [[2]; [4;6]]
let patienceTest5 () = patience [[2]; [6]; [10]] 8 = [[2]; [6]; [8; 10]]
let patienceTest6 () = patience [[2]; [6]; [10]] 12 = [[2]; [6]; [10]; [12]]
let patienceTest7 () = patience [[2]; [3;6]; [10]] 3 = [[2]; [3;3;6]; [10]]
let patienceTest8 () = patience [[2]; [3]; [4]; [5]; [6]] 4 = [[2]; [3]; [4;4]; [5]; [6]]
let patienceTests () =
  Lib.run_test "patience test1" patienceTest1 ;
  Lib.run_test "patience test2" patienceTest2 ;
  Lib.run_test "patience test3" patienceTest3 ;
  Lib.run_test "patience test4" patienceTest4 ;
  Lib.run_test "patience test5" patienceTest5 ;
  Lib.run_test "patience test6" patienceTest6 ;
  Lib.run_test "patience test7" patienceTest7 ;
  Lib.run_test "patience test8" patienceTest8


let isEven n = (n mod 2 = 0)

open FilteredSet

let rec insert_list xs s =
  match xs with
  | [] -> s
  | x :: xs -> insert_list xs (insert x s)

let filteredSetTests_wrapper () =
  let evenSet_empty = newSet isEven in

  let evenSet_1 = insert_list [1;2;3;4;5;6] evenSet_empty in
  let evenSet_2 = insert_list [10;12;13] evenSet_empty in

  let lt5Set_empty = newSet ((>) 5) in

  let lt5Set_1 = insert_list [1;2;3;4;5;6] lt5Set_empty in
  let lt5Set_2 = mapAndFilter ((+) 2) lt5Set_1 in
  let filteredSetTest1 () = member 5 evenSet_1 = false in
  let filteredSetTest2 () = member 2 evenSet_1 = true in
  let filteredSetTest3 () = member 2 evenSet_2 = false in
  let filteredSetTest4 () = member 12 evenSet_2 = true in
  let filteredSetTest5 () = member 4 lt5Set_1 = true in
  let filteredSetTest6 () = member 5 lt5Set_1 = false in
  let filteredSetTest7 () = member 6 lt5Set_2 = false in
  let filteredSetTest8 () = member 4 lt5Set_2 = true in
  let filteredSetTest9 () = member 1 lt5Set_2 = false in
  Lib.run_test "filteredSet test1" filteredSetTest1 ;
  Lib.run_test "filteredSet test2" filteredSetTest2 ;
  Lib.run_test "filteredSet test3" filteredSetTest3 ;
  Lib.run_test "filteredSet test4" filteredSetTest4 ;
  Lib.run_test "filteredSet test5" filteredSetTest5 ;
  Lib.run_test "filteredSet test6" filteredSetTest6 ;
  Lib.run_test "filteredSet test7" filteredSetTest7 ;
  Lib.run_test "filteredSet test8" filteredSetTest8 ;
  Lib.run_test "filteredSet test9" filteredSetTest9

let filteredSetTests () =
  try filteredSetTests_wrapper () with
  | Failure s -> print_endline ("filteredSet tests error: `" ^ s ^ "`\n")
  | e -> print_endline ("filteredSet tests error: `" ^ Printexc.to_string e ^ "`\n")

(******************************************************************)

(******************************************************************)

let test part =
  match part with
  | One   -> isDigitTests()
  | Two   -> newFileNameTests()
  | Three -> homePathTests()
  | Four  -> atoiTests()
  | Five  -> tokenizeTests()
  | Six   -> formatTreeTests()
  | Seven   -> subpalindromeTests ()
  | Eight   -> listComparisonsTests ()
  | Nine -> patienceTests ()
  | Ten  -> filteredSetTests ()

let run () =
  let () = test One in
  let () = test Two in
  let () = test Three in
  let () = test Four in
  let () = test Five in
  let () = test Six in
  let () = test Seven in
  let () = test Eight in
  let () = test Nine in
  let () = test Ten in
  ()

let () =
  if (Array.length Sys.argv = 2 && Sys.argv.(1) = "test") then
    run ()
  else
    ()
