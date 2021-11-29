let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.tokenize lexbuf in
  (* let _ = Printer.string_of_program program in *)
  (* let _ = Checker.check program in *)
  let m = Codegen.gen program in
  print_endline (Llvm.string_of_llmodule m)
