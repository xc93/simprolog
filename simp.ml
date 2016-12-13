(* main entry point to SimProlog *)

open List
open Main

let rules : rule list ref = ref []
let is_interactive = 0 = (Sys.command "[ -t 0 ]")

let _ = 
  (* Add here *)
  (if is_interactive
      then print_endline "\nWelcome to the SimProlog \n"
      else ()); 
  (****************************************************)
  try
  let lexbuf = Lexing.from_channel stdin in
  while true do
   (if is_interactive
    then (print_string ">>"; flush stdout));
    let result = Parser.main Lexer.token lexbuf in
   (match result
    with  Rule(t,ts) -> let r = (t,ts) in
            rules := r :: !rules;
            print_string "OK. I know ";
	    print_int (length !rules);
            print_string " rules now.\n";
       |  Inquery t ->  
  	    print_string "OK. I am thinking ...\n";
            let sols = solve [t] 
                             !rules 
                             !rules 
                             identity_subst 
                             [] 
                             (fun sols -> sols) in
	    let sols = dd_subst sols in
            print_string "I found ";
            print_int (length sols);
            print_string " solutions.\n";
            let variables_in_inquery = collect_variables_in_term t in
            print_sols (map (fun subst -> pick_subst subst variables_in_inquery) 
                            sols);
            print_string "\n";
       |  ClearComm ->
            rules := []; print_string "OK. I know nothing now.\n";
       |  ShowComm ->
            print_string "OK. I know the following rule(s).\n";
            print_rules !rules;
            print_string "\n";
    );
    print_newline(); flush stdout
    done
with Lexer.Eof -> exit 0
