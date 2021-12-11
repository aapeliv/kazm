(* Copied from ocaml source since this is only available in >4.11 *)
(* https://github.com/ocaml/ocaml/blob/cce52acc7c7903e92078e9fe40745e11a1b944f0/stdlib/list.ml#L272-L278 *)
let fold_left_map f accu l =
  let rec aux accu l_accu = function
    | [] -> accu, List.rev l_accu
    | x :: l ->
        let accu, x = f accu x in
        aux accu (x :: l_accu) l in
  aux accu [] l
