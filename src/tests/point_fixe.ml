open Eval
open Type


(* ==================== *)
(* TESTS DE L'ÉVALUATEUR *)
(* ==================== *)

let test_evaluator_typeur () =


  
  let fact_term =
    Fix ("fact", 
          Abs ("n", 
              IfZero (Var "n", Int 1, 
                      Mult (Var "n", App (Var "fact", Pred (Var "n")))))) in
  let term_normal = ltr_cbv_norm_with_limit fact_term 200 in
  print_string "Test factorielle:\n";
  print_string (print_term term_normal); (* Devrait retourner "fix fact -> fun n -> if n=0 then 1 else n * fact (pred n)" *)
  print_char '\n';
  (match infer_type fact_term [] with
    | Some ty -> print_string ("Type de la factorielle: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour la factorielle.\n");

  let fact_5 = App (fact_term, Int 5) in
  let term_normal = ltr_cbv_norm_with_limit fact_5 200 in
  print_string "Test factorielle 5:\n";
  print_string (print_term term_normal); (* Devrait retourner "120" *)
  print_char '\n';
  (match infer_type fact_5 [] with
    | Some ty -> print_string ("Type de la factorielle 5: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour la factorielle 5.\n")

let () =
print_string "========== TESTS FACTORIELLE ==========\n";
test_evaluator_typeur ();
print_string "\n========== FIN DES TESTS ==========\n";

