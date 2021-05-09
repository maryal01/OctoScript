module A = Ast
module S = Sast

module Set = Set.Make

(* Fixed or Var indicates normal parameters or varargs *)
type params = Fixed of A.typ list | Var of A.typ list



(* non-Static types will have their types checked by how the function is called *)
type rttype = Static of A.typ | Relative of int | ListElem of int | ListWithElem of int | TupleElem of int * int | TableElem of int * int

type builtin_func = string * rttype * rttype list
let builtins = 
   [

      (* return types shouldnt use ListElem type when a empty list at the position is possible *)
      (* ("length", Static A.INT, [Static (A.LIST None)]);
      ("get", ListElem 0, [Static (A.LIST None); Static A.INT]);
      ("add", ListWithElem 1, [Static (A.LIST None); ListElem 0]); *)
      ("concat", ListWithElem 1, [Static (A.LIST None); Static (A.LIST None)] );
      ("replace", ListWithElem 1, [Static (A.LIST None); Static A.INT; ListElem 0]);
      
      ("list_length", Static A.INT, [Static (A.LIST None)]);
      ("list_get", ListElem 0, [Static (A.LIST None); Static A.INT]);
      ("list_add", ListWithElem 1, [Static (A.LIST None); ListElem 0]);

      ("tuple_length", Static A.INT, [Static (A.TUPLE None)]);
      (* ("tuple_get", TupleElem (0, 0), [Static (A.TUPLE None); Static A.INT]);
       *)
      ("table_get", TableElem (0, 0), [Static (A.TABLE None); Static A.INT; Static A.INT]);
      ("table_size", Static A.INT, [Static (A.LIST None)]);
      ("table_get_row", Static (A.TUPLE None), [Static (A.TABLE None); Static A.INT]);
      ("table_get_col", Static (A.LIST None), [Static (A.TABLE None); Static A.INT]);
   ]

(* OctoScript name, C name, return type, parameter list *)
type predef_func = string * string * A.typ * params

(* Complex types with element type None won't have thier element types checked *)
let predefs = 
   [
      ("length", "length_tuple", A.INT, Fixed [A.TUPLE None]);
      ("print", "printf", A.INT, (Var [A.STRING]));
      ("test", "test", A.NONE, (Fixed [A.LAMBDA ([], A.INT)]));
      (* ("test_return_same", "test_return_same", A.LIST None, (Fixed [A.LIST None])); *)
      
      ("size", "size", A.NONE, (Fixed [A.TUPLE None]));
      
      ("read", "read", A.TABLE None, (Fixed [A.STRING; A.LIST (Some A.STRING); A.STRING]));
      ("write", "write", A.NONE, (Fixed [A.TABLE None; A.STRING; A.STRING]));
      

      (* to string functions *)
      ("string_of_list", "string_of_list", A.STRING, (Fixed [A.LIST None]));
      ("string_of_tuple", "string_of_tuple", A.STRING, (Fixed [A.TUPLE None]));
      ("intToString", "intToString", A.STRING, (Fixed [A.INT]));
      ("floatToString", "floatToString", A.STRING, (Fixed [A.FLOAT]));
      ("boolToString", "boolToString", A.STRING, (Fixed [A.BOOLEAN]));

      (* Standard C String library functions *)
      ("toLower", "toLower", A.STRING, (Fixed [A.STRING]));
      ("toUpper", "toUpper", A.STRING, (Fixed [A.STRING]));
      ("concat", "concat",   A.STRING, (Fixed [A.STRING; A.STRING]));
      ("substring", "substring", A.STRING, (Fixed [A.STRING; A.INT; A.INT]));
      ("strlen", "length", A.INT, (Fixed [A.STRING]));
      
      ("append", "append", A.LIST None, (Var [A.LIST None]));
      ("insert", "insert", A.LIST None, (Var [A.LIST None]));
      ("set", "set", A.LIST None, (Var [A.LIST None; A.INT]));
      ("countRows", "countRows", A.INT, (Fixed [A.TABLE None]));
      ("countCols", "countCols", A.INT, (Fixed [A.TABLE None]));
      ("copyTuple", "copyTuple", A.TUPLE None, (Fixed [A.TUPLE None]));

      ("tupleSet", "tupleSet", A.TUPLE None, (Var [A.TUPLE None; A.INT]));
      ("printTuple", "printTuple", A.NONE, (Fixed [A.TUPLE None]));
      ("print_list", "print_list", A.NONE, (Fixed [A.LIST None]));
  
      ("tuple_get", "tuple_get", A.LIST None, (Fixed [A.TUPLE None; A.INT]));

   ]



let predef_names = 
  List.map (fun (n,_,_,_)-> n) predefs

let builtin_names = 
  List.map (fun (n,_,_)-> n) builtins
