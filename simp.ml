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
  (* Add here *)
  (if is_interactive
    then (print_string ">>"; flush stdout));
  (*****************************************************)
  while true do
    let result = Parser.main Lexer.token lexbuf in
   (match result
    with  Rule(t,ts) -> let r = (t,ts) in
            rules := r :: !rules;
            print_string "OK. I know ";
	    print_int (length !rules);
            print_string " rules now.";
       |  Inquery t ->  
  	    print_string "OK. I am thinking ...\n";
            let sols = solve [t] 
                             !rules 
                             !rules 
                             identity_subst 
                             [] 
                             (fun sols -> sols) in
            print_string "I found ";
            print_int (length sols);
            print_string " solutions.\n";
            let variables_in_inquery = collect_variables_in_term t in
            print_sols (map (fun subst -> pick_subst subst variables_in_inquery) 
                            sols);
       |  ClearComm ->
            rules := []; print_string "OK. I know nothing now.";
       |  ShowComm ->
            print_string "OK. I know the following rule(s).\n";
            print_rules !rules;
    );
    print_newline(); flush stdout
    done
with Lexer.Eof -> exit 0
