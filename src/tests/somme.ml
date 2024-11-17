open Eval
open Type


(* ==================== *)
(* TESTS DE L'ÉVALUATEUR *)
(* ==================== *)

let test_evaluator_typeur () =
  let i = Abs ("x", Var "x") in

  (* (λx.sw x ▷ y : y 2 + y 3 4) (g : I ) *)
  let exemple = App (Abs ("x", Switch (Var "x", "y", App (Var "y", Add (Int 2, Int 3)), App (App (Var "y", Int 3), Int 4))), Left i) in
  print_string (print_term exemple) ;
  print_string " -> " ;
  print_string (print_term (ltr_cbv_norm_with_limit exemple 100)) ;
  print_newline () ;
  (match infer_type exemple [] with
    | Some ty -> print_string ("Type de l'exemple: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour l'exemple.\n");

  (* gauche *)
  let gauche = Left i in
  print_string (print_term gauche) ;
  print_string " -> " ;
  print_string (print_term (ltr_cbv_norm_with_limit gauche 100)) ;
  print_newline () ;
  (match infer_type gauche [] with
    | Some ty -> print_string ("Type de gauche: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour gauche.\n");

  (* droite *)
  let droite = Right i in
  print_string (print_term droite) ;
  print_string " -> " ;
  print_string (print_term (ltr_cbv_norm_with_limit droite 100)) ;
  print_newline () ;
  (match infer_type droite [] with
    | Some ty -> print_string ("Type de droite: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour droite.\n")


let () =
print_string "========== TESTS SOMME ==========\n";
test_evaluator_typeur ();
print_string "\n========== FIN DES TESTS ==========\n";

