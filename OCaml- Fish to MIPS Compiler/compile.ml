(* Compile Fish AST to MIPS AST *)
open Mips

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)


let temp_add (v: string) : unit =
    variables := VarSet.add v (!variables); ()

let rename_var (x: string) : string =
        "V" ^ x
    
(* find all of the variables in a program and add them to
 * the set variables *)
 let rec collect_vars (p : Ast.program) : unit = 
    (*************************************************************)
    (*raise IMPLEMENT_ME*)
    (*************************************************************)
  let rec collect_vars_exp (e : Ast.exp) : unit =
    let (r, _) = e in
    match r with
      Ast.Int _ -> ()
    | Ast.Var x -> temp_add x
    | Ast.Binop (e1, _, e2) -> collect_vars_exp e1; collect_vars_exp e2
    | Ast.Not e1 -> collect_vars_exp e1
    | Ast.And (e1, e2) -> collect_vars_exp e1; collect_vars_exp e2
    | Ast.Or (e1, e2) -> collect_vars_exp e1; collect_vars_exp e2
    | Ast.Assign (x, e1) -> temp_add x; collect_vars_exp e1 in
  let (s, _) = p in
  match s with
  | Ast.Exp e -> collect_vars_exp e
  | Ast.Seq (s1, s2) -> collect_vars s1; collect_vars s2
  | Ast.If (e, s1, s2) -> collect_vars_exp e; collect_vars s1; collect_vars s2
  | Ast.While (e, s1) -> collect_vars_exp e; collect_vars s1
  | Ast.For (e1, e2, e3, s1) -> collect_vars_exp e1; collect_vars_exp e2; collect_vars_exp e3; collect_vars s1
  | Ast.Return e -> collect_vars_exp e


(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
    let rec exp2mips ((e,_):Ast.exp) : inst list =
        (match e with
          Ast.Int x -> [Li(R2, Word32.fromInt x)]
        | Ast.Var x -> [La(R2, (rename_var x)); Lw(R2, R2, Int32.zero)]
        | Ast.Binop (e1, b, e2) ->
          (let t = new_temp() in
           (exp2mips e1) @ [La(R3,t); Sw(R2, R3, Int32.zero)] @
             (exp2mips e2) @ [La(R3, t); Lw(R3, R3, Int32.zero)] @
             (match b with
             | Ast.Plus  -> [Add(R2, R3, Reg R2)]
             | Ast.Minus -> [Sub(R2, R3, R2)]
             | Ast.Times -> [Mul(R2, R3, R2)]
             | Ast.Div -> [Div(R2, R3, R2)]
             | Ast.Eq -> [Seq(R2, R3, R2)]
             | Ast.Neq -> [Sne(R2, R3, R2)]
             | Ast.Lt -> [Slt(R2, R3, R2)]
             | Ast.Lte -> [Sle(R2, R3, R2)]
             | Ast.Gt -> [Sgt(R2, R3, R2)]
             | Ast.Gte -> [Sge(R2, R3, R2)]))
        | Ast.Not e1 -> (exp2mips e1) @ [Seq(R2, R2, R0)]
        | Ast.And (e1, e2) ->
          (let else_label = new_label() in
           let end_label = new_label() in
           (exp2mips e1) @ [Beq(R2, R0, else_label)] @
         (exp2mips e2) @ [Sne(R2, R2, R0); J end_label; Label else_label] @
         [Li(R2, Word32.fromInt 0); Label end_label])
        | Ast.Or (e1, e2) ->
          (let else_label = new_label() in
           let end_label = new_label() in
           (exp2mips e1) @ [Beq(R2, R0, else_label)] @
         [Li(R2, Word32.fromInt 1); J end_label; Label else_label] @
         (exp2mips e2) @ [Sne(R2, R2, R0); Label end_label])
        | Ast.Assign (x, e1) -> (exp2mips e1) @ [La(R3, (rename_var x)); Sw(R2, R3, Int32.zero)]) in
      match s with
      | Ast.Exp e -> exp2mips e
      | Ast.Seq (s1, s2) -> (compile_stmt s1) @ (compile_stmt s2)
      | Ast.If (e, s1, s2) ->
        (let else_label = new_label() in
         let end_label = new_label() in
         (exp2mips e) @ [Beq(R2, R0, else_label)] @
           (compile_stmt s1) @ [J end_label; Label else_label] @
           (compile_stmt s2) @ [Label end_label])
      | Ast.While (e, s1) ->
        (let test_label = new_label() in
         let top_label = new_label() in
         [J test_label; Label top_label] @ (compile_stmt s1) @
           [Label test_label] @ (exp2mips e) @ [Bne(R2, R0, top_label)])
      | Ast.For (e1, e2, e3, s1) ->
        compile_stmt (Ast.Seq ((Ast.Exp e1, 0), (Ast.While (e2, (Ast.Seq (s1, (Ast.Exp e3, 0)), 0)), 0)), 0)
      | Ast.Return e -> (exp2mips e) @ [Jr R31]
    
(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "fish_main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl printInt\n" ^
    "\t.globl fish_main\n" ^
    "\t.globl main\n\n" ^
    "main:\n" ^
    "\tmove $s8, $31\n" ^
    "\tjal fish_main\n" ^
    "\tmove $31, $s8\n" ^
    "\tmove $a0, $2\n" ^
    "\tj printInt\n\n" ^
    "printInt:\n" ^
    "\tadd $t0, $v0, $zero\n"^
    "\tli $v0, 1\n"^
    "\tsyscall\n"^
    "\tadd $v0, $t0, $zero\n"^
    "\tjr $ra\n\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"
