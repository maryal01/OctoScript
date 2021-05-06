module A = Ast
module S = Sast

module Set = Set.Make

(* Fixed or Var indicates normal parameters or varargs *)
type params = Fixed of A.typ list | Var of A.typ list



(* non-Static types will have their types checked by how the function is called *)
type rttype = Static of A.typ | Relative of int | ListElem of int | TupleElem of int * int | TableElem of int * int

type builtin_func = string * rttype * A.typ list
let builtins = 
   [
      ("length", Static A.INT, [A.LIST None]);
      ("get", ListElem 0, [A.LIST None; A.INT]);
      ("add", Static (A.LIST  (Some A.INT) ), [A.LIST None; A.INT])
   ]


(* OctoScript name, C name, return type, parameter list *)
type predef_func = string * string * A.typ * params

(* Complex types with element type None won't have thier element types checked *)
let predefs = 
   [
      ("print", "printf", A.INT, (Var [A.STRING]));
      
      ("test", "test", A.NONE, (Fixed [A.LAMBDA ([], A.INT)]));
      (* ("test_return_same", "test_return_same", A.LIST None, (Fixed [A.LIST None])); *)
      
      ("size", "size", A.NONE, (Fixed [A.TUPLE None]));
      
      ("read", "read", A.TABLE None, (Fixed [A.STRING; A.LIST None; A.BOOLEAN; A.STRING]));
      ("write", "write", A.NONE, (Fixed [A.TABLE None; A.STRING; A.BOOLEAN; A.STRING]));
      
      ("string_of_list", "string_of_list", A.STRING, (Fixed [A.LIST None]));
      ("string_of_tuple", "string_of_tuple", A.STRING, (Fixed [A.TUPLE None]));

      (* Standard C String library functions *)
      ("toLower", "toLower", A.STRING, (Fixed [A.STRING]));
      ("toUpper", "toUpper", A.STRING, (Fixed [A.STRING]));
      ("concat", "concat",   A.STRING, (Fixed [A.STRING; A.STRING]));
      ("substring", "substring", A.STRING, (Fixed [A.STRING; A.INT; A.INT]));
      ("strlen", "length", A.INT, (Fixed [A.STRING]));
      
      ("append", "append", A.LIST None, (Var [A.LIST None]));
      ("insert", "insert", A.LIST None, (Var [A.LIST None]));
      (* ("concat", "concatLists", A.LIST None, (Fixed [A.LIST None; A.LIST None])); *)
      ("set", "set", A.LIST None, (Var [A.LIST None; A.INT]));
      ("countRows", "countRows", A.INT, (Fixed [A.TABLE None]));
      ("countCols", "countCols", A.INT, (Fixed [A.TABLE None]));
      ("copyTuple", "copyTuple", A.TUPLE None, (Fixed [A.TUPLE None]));

      ("tupleSet", "tupleSet", A.TUPLE None, (Var [A.TUPLE None; A.INT]));
   ]



let predef_names = 
  List.map (fun (n,_,_,_)-> n) predefs

let builtin_names = 
  List.map (fun (n,_,_)-> n) builtins