open Eval
open Type


(* ==================== *)
(* TESTS DE L'ÉVALUATEUR *)
(* ==================== *)

let test_evaluator_typeur () =
  let produit : pterm = Prod (Int 3, Abs ("x", Int 4)) in
  let term_normal = ltr_cbv_norm_with_limit produit 200 in
  print_string "Test produit 3 4:\n";
  print_string (print_term term_normal); (* Devrait retourner "12" *)
  print_char '\n';
  (match infer_type produit [] with
    | Some ty -> print_string ("Type du produit: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour le produit.\n");

  let projd : pterm = ProjR (produit) in
  let term_normal = ltr_cbv_norm_with_limit projd 200 in
  print_string ("Test projection droite: " ^ (print_term produit) ^ ":\n");
  print_string (print_term term_normal); (* Devrait retourner "4" *)
  print_char '\n';
  (match infer_type projd [] with
    | Some ty -> print_string ("Type de la projection droite: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour la projection droite.\n");

  let projg : pterm = ProjL (produit) in
  let term_normal = ltr_cbv_norm_with_limit projg 200 in
  print_string ("Test projection gauche de " ^ (print_term produit) ^ ":\n");
  print_string (print_term term_normal); (* Devrait retourner "3" *)
  print_char '\n';
  (match infer_type projg [] with
    | Some ty -> print_string ("Type de la projection gauche: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour la projection gauche.\n");

  let i = Abs ("x", Var "x") in
  let k = Abs ("x", Abs ("y", Var "x")) in
  (* (λc.(projG c) (projD c)) (I , I K ) *)
  let exemple = App (Abs ("c", App (ProjL (Var "c"), ProjR (Var "c"))), Prod (i, App (i, k))) in
  let term_normal = ltr_cbv_norm_with_limit exemple 200 in
  print_string ("Test exemple: " ^ (print_term exemple) ^ " -> " ^ (print_term term_normal) ^ "\n");
  print_string (print_term term_normal); (* Devrait retourner "I" *)
  print_char '\n';
  (match infer_type exemple [] with
    | Some ty -> print_string ("Type de l'exemple: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour l'exemple.\n")

let () =
print_string "========== TESTS PRODUIT ==========\n";
test_evaluator_typeur ();
print_string "\n========== FIN DES TESTS ==========\n";

