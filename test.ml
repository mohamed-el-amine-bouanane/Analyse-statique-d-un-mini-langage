let read_lines (file:in_channel) : (int * string)list = 
  let rec read_lines_aux (file:in_channel) (acc:(int * string) list) (pos: int): (int * string) list =
    try
      let x = input_line file
      in if String.length x == 0 then read_lines_aux file acc (pos+1)
      else read_lines_aux file ((pos, x)::acc) (pos+1)
    with End_of_file -> acc
  in List.rev (read_lines_aux file [] 1)

let file = open_in "prog.p";;
let l = read_lines file;;

close_in file;;

let rec print_list = function
  | [] -> ()
  | h::r -> match h with x, y -> Printf.printf "Line %i = %s\n" x y; print_list r;;

print_list l;;
(* List.iter (Printf.printf ) l;; *)

let a = try
  (int_of_string "5f")
with Failure e -> -1

type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block;;

type expr_tree =
| Nil
| Node of expr_tree * string * expr_tree

(* let load_expr_tree (l : string list) : expr_tree =
  let rec load_expr_tree_aux (input_list : string list) : (exp_tree * string list) =
    match input_list with
    | [] -> Nil, []
    | hd :: tl ->
        if operateur hd then 
          let left, reste1 = load_prefix_notation_tree tl
          in let right, reste2 = load_prefix_notation_tree reste1
          in Node (left, hd, right), reste2 
        else 
          Node (Nil, hd, Nil), tl
  in load_expr_tree_aux *)

let operateur (s : string) : op option =
  if s = "+" then Some Add
  else if s = "-" then Some Sub
  else if s = "*" then Some Mul
  else if s = "/" then Some Div
  else if s = "%" then Some Mod
  else None;;

let read_expr (ll : string list) : expr =
  let rec read_expr_aux l = match l with 
  | [] -> failwith "Erreur de syntaxe: expression vide"
  | hd :: tl ->
    (match operateur hd with
      | None ->
        (match (int_of_string_opt hd) with
          | Some n -> Num n, tl
          | None -> if ((hd <> "+") && (hd <> "-") && (hd <> "*") && (hd <> "/") && (hd <> "%")) then Var hd, tl
            else failwith "Erreur de syntaxe: expression non reconnue")
      | Some op -> 
        let left, reste1 = read_expr_aux tl
        in let right, reste2 = read_expr_aux reste1
        in (Op (op, left, right), reste2))
      in fst (read_expr_aux ll);; 

read_expr ["+"; "+"; "3"; "4"; "5"];;