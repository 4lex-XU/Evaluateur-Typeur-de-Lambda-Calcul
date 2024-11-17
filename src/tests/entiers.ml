open Eval
open Type


(* ==================== *)
(* TESTS DE L'ÉVALUATEUR *)
(* ==================== *)

let test_evaluator_typeur () =
  let add = (Add(Int 3, Int 4)) in
  let term_normal = ltr_cbv_norm_with_limit add 200 in
  print_string "Test addition:\n";
  print_string (print_term term_normal); (* Devrait retourner "7" *)
  print_char '\n';
  (match infer_type add [] with
    | Some ty -> print_string ("Type de l'addition: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour l'addition.\n");

  let arith = (Add(Int 3, Mult(Int 4, Int 5))) in
  let term_normal = ltr_cbv_norm_with_limit arith 200 in
  print_string "Test addition et multiplication:\n";
  print_string (print_term term_normal); (* Devrait retourner "23" *)
  print_char '\n';
  (match infer_type arith [] with
    | Some ty -> print_string ("Type de l'addition et multiplication: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour l'addition et multiplication.\n");

  let succ = (Succ(Int 3)) in
  let term_normal = ltr_cbv_norm_with_limit succ 200 in
  print_string "Test succ:\n";
  print_string (print_term term_normal); (* Devrait retourner "4" *)
  print_char '\n';
  (match infer_type succ [] with
    | Some ty -> print_string ("Type de succ: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour succ.\n");
  
  let pred = (Pred(Int 3)) in
  let term_normal = ltr_cbv_norm_with_limit pred 200 in
  print_string "Test pred:\n";
  print_string (print_term term_normal); (* Devrait retourner "2" *)
  print_char '\n';
  (match infer_type pred [] with
    | Some ty -> print_string ("Type de pred: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour pred.\n");

  let if_zero = (IfZero(Int 0, Int 1, Int 2)) in
  let term_normal = ltr_cbv_norm_with_limit if_zero 200 in
  print_string "Test if_zero:\n";
  print_string (print_term term_normal); (* Devrait retourner "1" *)
  print_char '\n';
  (match infer_type if_zero [] with
    | Some ty -> print_string ("Type de if_zero: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour if_zero.\n")


let () =
print_string "========== TESTS ENTIERS ==========\n";
test_evaluator_typeur ();
print_string "\n========== FIN DES TESTS ==========\n";
    