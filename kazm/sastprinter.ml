let () =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.tokenize lexbuf in 
    let sast = Checker.check ast in
    print_string (Sast.string_of_sprogram sast)
