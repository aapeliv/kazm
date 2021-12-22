module L = Llvm

let context = L.global_context()

let i1_t = L.i1_type context in
let i8_t = L.i8_type context in
let i32_t = L.i32_type context in
let double_t = L.double_type context in
let char_t = i8_t in
let void_t = L.void_type context in
let char_ptr_t = L.pointer_type char_t in

let struct_node_type: L.lltype = L.named_struct_type context "Node"
(*  
  struct Node {
    Node *next;
    void *data;
  }
*)

let struct_list_type: L.lltype = L.named_struct_type context "List"
(*
  struct List {
    Node *head;
  }
*)
(* compute length by iterating through all the nodes *)

(* Definition of a list type *)
let _ = 
  L.struct_set_body struct_list_type
  [| L.pointer_type struct_node_type; |] false (* Node *head *)

(* Definition of a Node type *)
let _ = 
  L.struct_set_body struct_node_type
  [|
    L.pointer_type struct_node_type; (* Node *next *)
    L.pointer_type void_t            (* Void *data *)
  |] false


(*  arr.c - John Recommends: *)

let struct_arr_type : L.lltype = L.named_struct_type context "Arr"
(*
  struct Arr {
    int len;
    int capacity;
    void *data;
  }
*)
let _ = 
  L.struct_set_body struct_Arr_type
  [|
    i32_t;                  (* int len       *)
    i32_t;                  (* int capacity  *)
    L.pointer_type void_t   (* Void *        *)
  |] false


