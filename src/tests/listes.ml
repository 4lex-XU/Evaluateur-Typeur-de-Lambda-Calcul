open Eval
open Type


(* ==================== *)
(* TESTS DE L'ÉVALUATEUR *)
(* ==================== *)

let test_evaluator_typeur () =
  let nil = Nil in
  let term_normal = ltr_cbv_norm_with_limit nil 200 in
  print_string "Test liste vide:\n";
  print_string (print_term term_normal); (* Devrait retourner "[]" *)
  print_char '\n';
  (match infer_type nil [] with
    | Some ty -> print_string ("Type de la liste vide: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour la liste vide.\n");

  let cons = Cons(Int 3, Cons(Int 4, Nil)) in
  let term_normal = ltr_cbv_norm_with_limit cons 200 in
  print_string "Test liste [3; 4]:\n";
  print_string (print_term term_normal); (* Devrait retourner "3::4::[]" *)
  print_char '\n';
  (match infer_type cons [] with
    | Some ty -> print_string ("Type de la liste [3; 4]: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour la liste [3; 4].\n");

  let head = Head(cons) in
  let term_normal = ltr_cbv_norm_with_limit head 200 in
  print_string "Test head [3; 4]:\n";
  print_string (print_term term_normal); (* Devrait retourner "3" *)
  print_char '\n';
  (match infer_type head [] with
    | Some ty -> print_string ("Type de head [3; 4]: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour head [3; 4].\n");

  let tail = Tail(cons) in
  let term_normal = ltr_cbv_norm_with_limit tail 200 in
  print_string "Test tail [3; 4]:\n";
  print_string (print_term term_normal); (* Devrait retourner "4::[]" *)
  print_char '\n';
  (match infer_type tail [] with
    | Some ty -> print_string ("Type de tail [3; 4]: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour tail [3; 4].\n");

  let map = Fix ("map", 
                 Abs ("f", 
                     Abs ("l", 
                         IfEmpty (Var "l", Nil, 
                                  Cons (App (Var "f", Head (Var "l")), 
                                        App (App (Var "map", Var "f"), Tail (Var "l"))))))) in
  let term_normal = ltr_cbv_norm_with_limit map 200 in
  print_string "Test map:\n";
  print_string (print_term term_normal); (* Devrait retourner "fix map -> fun f -> fun l -> if l=[] then [] else f (head l)::map f (tail l)" *)
  print_char '\n';
  (match infer_type map [] with
    | Some ty -> print_string ("Type de map: " ^ (print_type ty) ^ "\n") (*type : ('a -> 'b) -> 'a list -> 'b list*)
    | None -> print_string "Erreur de typage pour map.\n");

  let next = Abs ("x", Succ (Var "x")) in
  let term_normal = ltr_cbv_norm_with_limit (App (App (map, next), cons)) 200 in
  print_string "Test map succ [3; 4]:\n";
  print_string (print_term term_normal); (* Devrait retourner "4::5::[]" *)
  print_char '\n';
  (match infer_type (App (App (map, next), cons)) [] with
    | Some ty -> print_string ("Type de map succ [3; 4]: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour map succ [3; 4].\n");

  let let_cons = 
    Let("l",
      Cons (Int 3, Cons (Int 4, Nil)),
      Var "l"
    )
  in
  let term_normal = ltr_cbv_norm_with_limit let_cons 200 in
  print_string "Test let cons:\n";
  print_string (print_term term_normal); (* Devrait retourner "3::4::[]" *)
  print_char '\n';
  (match infer_type let_cons [] with
    | Some ty -> print_string ("Type de let cons: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour let cons.\n");

  let double_map =
    App(
      App(map, Abs("x", Add(Var "x", Int 1))),
      Cons(Int 1, Cons(Int 2, Nil))
    )
  in
  let term_normal = ltr_cbv_norm_with_limit double_map 200 in
  print_string "Test double map:\n";
  print_string (print_term term_normal); (* Devrait retourner "2::3::[]" *)
  print_char '\n';
  (match infer_type double_map [] with
    | Some ty -> print_string ("Type de double map: " ^ (print_type ty) ^ "\n")
    | None -> print_string "Erreur de typage pour double map.\n")

let () =
print_string "========== TESTS LISTES ==========\n";
test_evaluator_typeur ();
print_string "\n========== FIN DES TESTS ==========\n";
    