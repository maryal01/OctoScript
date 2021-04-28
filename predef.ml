module A = Ast
module S = Sast

module Set = Set.Make

(* Fixed or Var indicates normal parameters or varargs *)
type params = Fixed of A.typ list | Var of A.typ list

(* OctoScript name, C name, return type, parameter list *)
type predef_func = string * string * A.typ * params


let predefs = 
  [("print", "printf", A.INT, (Var [A.STRING]));
   ("test", "test", A.NONE, (Fixed [A.LIST]));
   ("get", "get", A.INT, (Fixed [A.TUPLE; A.INT]));
   ("size", "size", A.NONE, (Fixed [A.TUPLE]));
   ("read", "read", A.TABLE, (Fixed [A.STRING; A.LIST; A.BOOLEAN; A.STRING]));
   ("tup", "tup", A.NONE, (Fixed [A.TUPLE]))]

  
   ("string_of_list", "string_of_list", A.STRING, (Fixed [A.LIST]));]

let predef_names = 
  List.map (fun (n,_,_,_)-> n) predefs