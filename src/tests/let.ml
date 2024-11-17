open Eval
open Type


(* ==================== *)
(* TESTS DE L'ÉVALUATEUR *)
(* ==================== *)

let test_evaluator_typeur () =
  let func = 
    Let("i",
        Abs("x", Var "x"),
        App(App (Var "i", Var "i"), Int 42)
      ) in
  let term_normal = ltr_cbv_norm_with_limit func 200 in
  print_string "Test let:\n";
  print_string (print_term term_normal); (* Devrait retourner "42" *)
  print_char '\n';
  (match infer_type func [] with
    | Some ty -> print_string ("Type de let: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour let.\n");
  let let_abs = 
    Let("id",
        Abs("x", Var "x"),
        Let("f",
            Abs("x", App(Var "id", Var "x")),
            App(Var "f", Int 42)
          )
      ) in
  let term_normal = ltr_cbv_norm_with_limit let_abs 200 in
  print_string "Test let abs:\n";
  print_string (print_term term_normal); (* Devrait retourner "42" *)
  print_char '\n';
  (match infer_type let_abs [] with
    | Some ty -> print_string ("Type de let abs: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour let abs.\n");
  let let_simple =
    Let("x",
        Int 42,
        Var "x"
      ) in
  let term_normal = ltr_cbv_norm_with_limit let_simple 200 in
  print_string "Test let simple:\n";
  print_string (print_term term_normal); (* Devrait retourner "42" *)
  print_char '\n';
  (match infer_type let_simple [] with
    | Some ty -> print_string ("Type de let simple: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour let simple.\n")

let () =
print_string "========== TESTS LET ==========\n";
test_evaluator_typeur ();
print_string "\n========== FIN DES TESTS ==========\n";
      