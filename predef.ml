module A = Ast
module S = Sast

module Set = Set.Make

(* Fixed or Var indicates normal parameters or varargs *)
type params = Fixed of A.typ list | Var of A.typ list

(* OctoScript name, C name, return type, parameter list *)
type predef_func = string * string * A.typ * params

let predefs = 
  [("print", "printf", A.INT, (Var [A.STRING]));
   ("test", "test", A.NONE, (Fixed [A.LIST (A.INT)]))]

let predef_names = 
  List.map (fun (n,_,_,_)-> n) predefs