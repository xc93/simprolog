(* main entry point to SimProlog *)

open List
open Main

let rules : rule list ref = ref []

let _ =
   try
     let lexbuf = Lexing.from_channel stdin in
     while true do
       let result = Parser.main Lexer.token lexbuf in
         (* Modify here: result is of type Main.command *)
	(match result
         with  Rule(t,ts) -> let r = (t,ts) in
                             rules := r :: !rules;
                             print_string "OK. I know ";
			     print_int (length !rules);
                             print_string " rules now."
            |  Inquery t ->  
  	       print_string "OK. I am thinking ...\n";
               let sols = solve [t] !rules !rules identity_subst [] (fun sols -> sols) in
               print_string "I found ";
               print_int (length sols);
               print_string " solutions.\n";
               print_sols (map (fun subst -> pick_subst subst (collect_variables_in_term t)) sols)
        );
         print_newline(); flush stdout
     done
   with Lexer.Eof ->
     exit 0
