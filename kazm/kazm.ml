let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.tokenize lexbuf in
  let output = Printer.string_of_program program in
  ignore (Checker.check program); print_endline output; print_endline "Done."
