module A = Ast
module S = Sast

module Set = Set.Make

(* Fixed or Var indicates normal parameters or varargs *)
type params = Fixed of A.typ list | Var of A.typ list

(* OctoScript name, C name, return type, parameter list *)
type predef_func = string * string * A.typ * params


let predefs = 
  [("print", "printf", A.INT, (Var [A.STRING]));
   (* ("test", "test", A.NONE, (Fixed [A.LIST])); *)
   ("get", "get", A.INT, (Fixed [A.LIST; A.INT]));
   ("size", "size", A.NONE, (Fixed [A.TUPLE]));
   ("read", "read", A.TABLE, (Fixed [A.STRING; A.LIST; A.BOOLEAN; A.STRING]));
   ("write", "write", A.NONE, (Fixed [A.TABLE; A.STRING; A.BOOLEAN; A.STRING]));
   ("string_of_list", "string_of_list", A.STRING, (Fixed [A.LIST]));
   ("string_of_tuple", "string_of_tuple", A.STRING, (Fixed [A.TUPLE]));
   ("toLower", "toLower", A.STRING, (Fixed [A.STRING]));
   ("toUpper", "toUpper", A.STRING, (Fixed [A.STRING]));
   ("concat", "concat",   A.STRING, (Fixed [A.STRING; A.STRING]));
   ("substring", "substring", A.STRING, (Fixed [A.STRING; A.INT; A.INT]));
   ("length", "length", A.INT, (Fixed [A.LIST]));
   ("append", "append", A.LIST, (Var [A.LIST]));
   ("insert", "insert", A.LIST, (Var [A.LIST]));
   ("concat", "concatLists", A.LIST, (Fixed [A.LIST; A.LIST]));
   ("set", "set", A.LIST, (Var [A.LIST; A.INT]));
   ("countRows", "countRows", A.INT, (Fixed [A.TABLE]));
   ("countCols", "countCols", A.INT, (Fixed [A.TABLE]));
   ("copyTuple", "copyTuple", A.TUPLE, (Fixed [A.TUPLE]));
   
   ("tupleSet", "tupleSet", A.TUPLE, (Var [A.TUPLE; A.INT]))]


let predef_names = 
  List.map (fun (n,_,_,_)-> n) predefs