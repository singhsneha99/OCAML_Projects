open Mips_ast
open Mips_assem
open Byte

exception TODO
exception FatalError



(* Take a look at the definition of the Mips AST and machine state in mips_ast.ml *)

(* Given a starting state, simulate the Mips machine code to get a final state;
   a final state is reached if the the next instruction pointed to by the PC is
   all 0s.
 *)


 let rec interp (init_state : state) : state =
  let pc = init_state.pc in
  let cur_inst = mem_lookup pc init_state.m in
  if cur_inst = mk_byte 0l then
    init_state (* Stop execution when the next instruction is all 0s *)
  else
    let cur_word = read_word init_state.m pc in
    let next_pc = Int32.add pc 4l in
    let next_state = { init_state with pc = next_pc } in
    
    (* Extract the opcode from the instruction *)
    let opcode = Int32.shift_right_logical cur_word 26 in

    let instformat =
      match opcode with
      | 0l -> (* R-type opcode *)
        let rfun = Int32.logand cur_word (ones 6) in
        let r_rs = Int32.shift_right_logical (Int32.logand cur_word (ones 5)) 21 in
        let r_rt = Int32.shift_right_logical (Int32.logand cur_word (ones 5)) 16 in
        let r_rd = Int32.shift_right_logical (Int32.logand cur_word (ones 5)) 11 in
        let r_shamt = Int32.shift_right_logical (Int32.logand cur_word (ones 5)) 6 in
        R { r_opcode = opcode; r_rs; r_rt; r_rd; r_shamt; r_fun = rfun }
      | 0x4l -> (* I-type opcode for Beq *)
        let i_rs = Int32.shift_right_logical (Int32.logand cur_word (ones 5)) 21 in
        let i_rt = Int32.shift_right_logical (Int32.logand cur_word (ones 5)) 16 in
        let i_imm = Int32.logand cur_word (ones 16) in
        I { i_opcode = opcode; i_rs; i_rt; i_imm }
      | 0x3l -> (* J-type opcode for Jal *)
        let j_addr = Int32.logand cur_word (ones 26) in
        J { j_opcode = opcode; j_addr }
      (* Add cases for other opcodes here *)

      | _ -> raise FatalError
    in

    let instruction = instformat2ins instformat in

    match instruction with
    | Add (rd, rs, rt) ->
        let value_rs = rf_lookup (reg2ind rs) next_state.r in
        let value_rt = rf_lookup (reg2ind rt) next_state.r in
        let value = Int32.add value_rs value_rt in
        let updated_rf = rf_update (reg2ind rd) value next_state.r in
        interp { next_state with r = updated_rf }

    | Beq (rs, rt, offset) ->
        let value_rs = rf_lookup (reg2ind rs) next_state.r in
        let value_rt = rf_lookup (reg2ind rt) next_state.r in
        if value_rs = value_rt then
          interp { next_state with pc = Int32.add pc (Int32.shift_left offset 2) }
        else
          interp next_state

    | Jr rs ->
        let value_rs = rf_lookup (reg2ind rs) next_state.r in
        interp { next_state with pc = value_rs }

    | Jal addr ->
        let updated_rf = rf_update 31 next_pc next_state.r in
        interp { next_state with pc = Int32.add pc (Int32.shift_left addr 2); r = updated_rf }

    | Li (rd, imm) ->
        let updated_rf = rf_update (reg2ind rd) imm next_state.r in
        interp { next_state with r = updated_rf }

    | Lui (rt, imm) ->
        let value = Int32.shift_left imm 16 in
        let updated_rf = rf_update (reg2ind rt) value next_state.r in
        interp { next_state with r = updated_rf }

    | Ori (rt, rs, imm) ->
        let value_rs = rf_lookup (reg2ind rs) next_state.r in
        let value = Int32.logor value_rs imm in
        let updated_rf = rf_update (reg2ind rt) value next_state.r in
        interp { next_state with r = updated_rf }

   (* | Lw (rt, rs, imm) ->
        let value_rs = rf_lookup (reg2ind rs) next_state.r in
        let mem_addr = Int32.add value_rs imm in
        let value = mem_lookup mem_addr next_state.m in
        let updated_rf = rf_update (reg2ind rt) value next_state.r in
        interp next_state

    | Sw (rt, rs, imm) ->
        let value_rs = rf_lookup (reg2ind rs) next_state.r in
        let value_rt = rf_lookup (reg2ind rt) next_state.r in
        let mem_addr = Int32.add value_rs imm in
        let updated_mem = mem_update mem_addr value_rt next_state.m in
        interp { next_state with m = updated_mem }*)

    | _ -> raise FatalError (* Handle unsupported instructions *)

(* Start execution from the initial state *)
let start_interp (prog : program) : state =
  interp (assem prog)


(*
  Here are a few details/assumptions about the assembler and interpreter that the autograder makes:
  * > Big Endian Encoding
  * > Program Data is stored starting at 0x400000
  * > Stack grows downward starting at 0x7ffffffc
  * > GP points to 30000000
  * > The assembler uses register 1 as temp storage for encoding Li
  * > We don't implement delay slots in either assembly or bitcode semantics
  * > As stated in lecture, we shift jump and break immediates left by 2
  * > The PC is always incremented before executing an instruction
  * > Beq subtracts 4 from the PC before adding its offset
  * > We preserve the top 4 bits of the PC when executing a jal
*)