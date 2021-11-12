let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.tokenize lexbuf in
  match program with
    PPair(_, st) -> print_endline st;
  print_endline ""; print_endline "Done."
