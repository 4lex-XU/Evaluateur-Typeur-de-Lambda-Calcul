open Eval
open Type


(* ==================== *)
(* TESTS DE L'ÉVALUATEUR *)
(* ==================== *)

let test_evaluator_typeur () =
  (* Identité *)
  let i = Abs ("x", Var "x") in
  let term_normal = ltr_cbv_norm_with_limit i 200 in
  print_string "Test identité (i):\n";
  print_string (print_term term_normal); (* Devrait retourner "fun x -> x" *)
  print_char '\n';
  (match infer_type i [] with
    | Some ty -> print_string ("Type de l'identité: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour l'identité.\n");

  (* Delta *)
  let delta = Abs ("x", App (Var "x", Var "x")) in
  let term_normal = ltr_cbv_norm_with_limit delta 200 in
  print_string "Test delta:\n";
  print_string (print_term term_normal); (* Devrait retourner "fun x -> x x" *)
  print_char '\n';
  (match infer_type delta [] with
    | Some ty -> print_string ("Type de delta: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour delta.\n");

  (* Omega *)
  let omega = App (Abs ("x", App (Var "x", Var "x")), Abs ("x", App (Var "x", Var "x"))) in
  (try
    print_string "Test omega:\n";
    let term_normal = ltr_cbv_norm_with_limit omega 100 in
    print_string (print_term term_normal);
    print_char '\n'
  with Timeout ->
    print_string "La réduction a divergé (Timeout).\n");
  (match infer_type omega [] with
    | Some ty -> print_string ("Type de omega: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour omega.\n");

  (* S combinator *)

  let s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))) in
  let term_normal = ltr_cbv_norm_with_limit s 200 in
  print_string "Test s combinator:\n";
  print_string (print_term term_normal); (* Devrait retourner "fun x -> fun y -> fun z -> x z (y z)" *)
  print_char '\n';
  (match infer_type s [] with
    | Some ty -> print_string ("Type de s: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour s.\n");

  (* K combinator *)

  let k = Abs ("x", Abs ("y", Var "x")) in
  let term_normal = ltr_cbv_norm_with_limit k 200 in
  print_string "Test k combinator:\n";
  print_string (print_term term_normal); (* Devrait retourner "fun x -> fun y -> x" *)
  print_char '\n';
  (match infer_type k [] with
    | Some ty -> print_string ("Type de k: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour k.\n");


  let skk = App (App (s, k), k) in
  let term_normal = ltr_cbv_norm_with_limit skk 200 in
  print_string "Test s k k:\n";
  print_string (print_term term_normal); (* Devrait retourner "fun z -> z" *)
  print_char '\n';
  (match infer_type skk [] with
    | Some ty -> print_string ("Type de skk: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour skk.\n");

  let sii = App (App (s, i), i) in
  let term_normal = ltr_cbv_norm_with_limit sii 200 in
  print_string "Test s i i:\n";
  print_string (print_term term_normal); (* Devrait retourner "fun y -> y" *)
  print_char '\n';
  (match infer_type sii [] with
    | Some ty -> print_string ("Type de sii: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour sii.\n");

  let delta_id = App (delta, i) in
  let term_normal = ltr_cbv_norm_with_limit delta_id 200 in
  print_string "Test delta i:\n";
  print_string (print_term term_normal); (* Devrait retourner "fun x -> x" *)
  print_char '\n';
  (match infer_type delta_id [] with
    | Some ty -> print_string ("Type de delta_id: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour delta_id.\n");

  let delta_delta = App (delta, delta) in
  (try
    print_string "Test delta delta:\n";
    let term_normal = ltr_cbv_norm_with_limit delta_delta 200 in
    print_string (print_term term_normal);
    print_char '\n'
  with Timeout ->
    print_string "La réduction a divergé (Timeout).\n");
  (match infer_type delta_delta [] with
    | Some ty -> print_string ("Type de delta_delta: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour delta_delta.\n");

  let kii = App (App (k, i), i) in
  let term_normal = ltr_cbv_norm_with_limit kii 200 in
  print_string "Test k i i:\n";
  print_string (print_term term_normal); (* Devrait retourner "fun y -> y" *)
  print_char '\n';
  (match infer_type kii [] with
    | Some ty -> print_string ("Type de kii: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour kii.\n");

(* Encodage de 0 en nombre de Church *)
  let zero = Abs ("f", Abs ("x", Var "x")) in 
  let term_normal = ltr_cbv_norm_with_limit zero 200 in
  let result = print_term term_normal in
  print_string "test zero\n";
  print_string result;  (* Devrait retourner "fun f -> fun x -> x" *)
  print_char('\n');
  (match infer_type zero [] with
    | Some ty -> print_string ("Type de zero: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour zero.\n");

  (* Encodage de 1 en nombre de Church *)
  let one = Abs ("f", Abs ("x", App (Var "f", Var "x"))) in
  let term_normal = ltr_cbv_norm_with_limit one 200 in
  let result = print_term term_normal in
  print_string "test one\n";
  print_string result;  (* Devrait retourner "fun f -> fun x -> f x" *)
  print_char('\n');
  (match infer_type one [] with
    | Some ty -> print_string ("Type de one: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour one.\n");

  (* Encodage de 2 en nombre de Church *)
  let two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x")))) in
  let term_normal = ltr_cbv_norm_with_limit two 200 in
  let result = print_term term_normal in
  print_string "test two\n";
  print_string result;  (* Devrait retourner "fun f -> fun x -> f (f x)" *)
  print_char('\n');
  (match infer_type two [] with
    | Some ty -> print_string ("Type de two: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour two.\n");

  (* Encodage de 3 en nombre de Church *)
  let three = Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x"))))) in
  let term_normal = ltr_cbv_norm_with_limit three 200 in
  let result = print_term term_normal in
  print_string "test three\n";
  print_string result;  (* Devrait retourner "fun f -> fun x -> f (f (f x))" *)
  print_char('\n');
  (match infer_type three [] with
    | Some ty -> print_string ("Type de three: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour three.\n");

  (* Encodage de 4 en nombre de Church *)

  (* fonction d'addition *)
  let add = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x")))))) in 
  (* Exemple test de normalisation : addition de 1 et 1 *)
  let term = App (App (add, one), one) in
  let result = ltr_cbv_norm_with_limit term 200 in
  print_string "test add 1 1\n";
  print_string (print_term result);  (* Devrait donner 2 *)
  print_char('\n');
  (match infer_type add [] with
    | Some ty -> print_string ("Type de add: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour add.\n");

  (* Fonction de multiplication en lambda-calcul *)
  let mul = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f"))))) in
  (* Exemple de test de normalisation : multiplication de 1 et 2 *)
  let term = App (App (mul, one), two) in
  let result = ltr_cbv_norm_with_limit term 200 in
  print_string "test mul 1 2\n";
  print_string (print_term result);  (* Devrait donner 2 *)
  print_char('\n');
  (match infer_type mul [] with
    | Some ty -> print_string ("Type de mul: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour mul.\n");

  (* Fonction d'exponentiation en lambda-calcul *)
  let exp = Abs ("m", Abs ("n", App (Var "n", Var "m"))) in
  (* Exemple de test de normalisation : 2 exposant 0 *)
  let term = App (App (exp, two), zero) in
  let result = ltr_cbv_norm_with_limit term 200 in
  print_string "test exp 2 0\n";
  print_string (print_term result);  (* Devrait donner 1 *)
  print_char('\n');
  (match infer_type exp [] with
    | Some ty -> print_string ("Type de exp: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour exp.\n");

  (* Fonction d'incrémentation en lambda-calcul *)
  let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))
  in 
  (* exemple de test de normalisation : successeur de 1 *)
  let term = App (succ, one) in
  let result = ltr_cbv_norm_with_limit term 200 in
  print_string "test succ 1\n";
  print_string (print_term result);  (* Devrait donner 2 *)
  print_char('\n');
  (match infer_type succ [] with
    | Some ty -> print_string ("Type de succ: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de type pour succ.\n");

  (* Fonction de prédécesseur en lambda-calcul *)
  let pred = Abs ("n", Abs ("f", Abs ("x", App (App (App (Var "n", Abs ("g", Abs ("h", App (Var "h", App (Var "g", Var "f"))))), Abs ("u", Var "x")), Abs ("u", Var "u")))))
  in
  (* exemple de test de normalisation : prédécesseur de 2 *)
  let term = App (pred, two) in
  let result = ltr_cbv_norm_with_limit term 200 in
  print_string "test pred 2\n";
  print_string (print_term result);  (* Devrait donner 1 *)
  print_char('\n');
  (match infer_type pred [] with
    | Some ty -> print_string ("Type de pred: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de type pour pred.\n")


let () =
  print_string "========== TESTS TYPES SIMPLES ==========\n";
  test_evaluator_typeur ();
  print_string "\n========== FIN DES TESTS ==========\n";
