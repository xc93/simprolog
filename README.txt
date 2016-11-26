# simprolog
Implementation of a simple version of Prolog in OCaml. This project includes a grammar, lexer, parser, semantics, evaluator (including back-tracking).


The simple version that is considered in this OCaml implementation is called SimProlog. 

Quantified Rules:
	qrls = [rl1; rl2; rl3; rl4]
Rule Instance:
	get_instance_rule qrl
Inquery List:
	inqs = [inq1; inq2; inq3]

How to store the process of unification?

Use data structures!

Proof(judgment, UsingRule(qrl, rl), proof list)

Task(task_status, judgment, usingrl, task list)
task_status: Todo, InProgress, Succeeded, Failed
