(* --------------- Evaluateur pour un λ calcul pur ---------------*)
exception Timeout

(* --------------- AST TERMS ---------------*)
type pterm =  Var of string
            | App of pterm * pterm 
            | Abs of string * pterm
            (* Entiers *)
            | Int of int
            | Add of pterm * pterm
            | Sub of pterm * pterm
            | Mult of pterm * pterm
            | Succ of pterm
            | Pred of pterm
            (* Listes *)
            | Nil
            | Cons of pterm * pterm
            | Head of pterm
            | Tail of pterm
            (* If *)
            | IfZero of pterm * pterm * pterm
            | IfEmpty of pterm * pterm * pterm
            (* Point fixe et let *)
            | Fix of string * pterm
            | Let of string * pterm * pterm
            (* Types Sommes *)
            | Left of pterm
            | Right of pterm
            | Switch of pterm * string * pterm * pterm
            (* Types Produits *)
            | Prod of pterm * pterm
            | ProjL of pterm      
            | ProjR of pterm
;;

(* Pretty printer pour afficher les termes *)
let rec print_term (t : pterm) : string =
    match t with
    | Var x -> x
    | App (t1, t2) -> "(" ^ print_term t1 ^ " " ^ print_term t2 ^ ")"
    | Abs (x, t1) -> "(fun " ^ x ^ " -> " ^ print_term t1 ^ ")"
    (* Entiers *)
    | Int n -> string_of_int n
    | Add (t1, t2) -> "(" ^ print_term t1 ^ " + " ^ print_term t2 ^ ")"
    | Sub (t1, t2) -> "(" ^ print_term t1 ^ " - " ^ print_term t2 ^ ")"
    | Mult (t1, t2) -> "(" ^ print_term t1 ^ " * " ^ print_term t2 ^ ")"
    | Succ t -> "succ(" ^ print_term t ^ ")"
    | Pred t -> "pred(" ^ print_term t ^ ")"
    (* Listes *)
    | Nil -> "[]"
    | Cons (t1, t2) -> "(" ^ print_term t1 ^ " :: " ^ print_term t2 ^ ")"
    | Head t1 -> "head(" ^ print_term t1 ^ ")"
    | Tail t1 -> "tail(" ^ print_term t1 ^ ")"
    (* If *)
    | IfZero (cond, t1, t2) -> 
        "if (" ^ print_term cond ^ " == 0) then " ^ print_term t1 ^ " else " ^ print_term t2
    | IfEmpty (cond, t1, t2) ->
        "if empty(" ^ print_term cond ^ ") then " ^ print_term t1 ^ " else " ^ print_term t2
    (* Point fixe / let *)
    | Fix (f, t1) -> "fix (" ^ f ^ " -> " ^ print_term t1 ^ ")"
    | Let (x, t1, t2) -> "let " ^ x ^ " = " ^ print_term t1 ^ " in " ^ print_term t2
    (* Types Sommes *)
    | Left t1 -> "Left(" ^ print_term t1 ^ ")"
    | Right t1 -> "Right(" ^ print_term t1 ^ ")"
    | Switch (t1, x, tG, tD) -> 
        "switch (" ^ print_term t1 ^ ") { Left(" ^ x ^ ") -> " ^ print_term tG ^ " | Right(" ^ x ^ ") -> " ^ print_term tD ^ " }"
    (* Types Produits *)
    | Prod (t1, t2) -> "(" ^ print_term t1 ^ ", " ^ print_term t2 ^ ")"
    | ProjL t1 -> "ProjL(" ^ print_term t1 ^ ")"
    | ProjR t1 -> "ProjR(" ^ print_term t1 ^ ")"
  
let compteur_var : int ref = ref 0

let nouvelle_var () : string = 
  compteur_var := !compteur_var + 1;
  "X" ^ ( string_of_int !compteur_var )
;;

(* Fonction de substitution *)
let rec substitution (x : string) (n : pterm) (t : pterm) : pterm =
  match t with
  | Var y -> if x = y then n else t
  | App (t1, t2) -> App (substitution x n t1, substitution x n t2)
  | Abs (y, t1) ->
      if x = y then Abs (y, t1)  (* Pas de substitution si la variable est liée *)
      else Abs (y, substitution x n t1)

  | Int _ -> t
  | Add (t1, t2) -> Add (substitution x n t1, substitution x n t2)
  | Sub (t1, t2) -> Sub (substitution x n t1, substitution x n t2)
  | Mult (t1, t2) -> Mult (substitution x n t1, substitution x n t2)

  | Succ t ->  Succ (substitution x n t)
  | Pred t -> Pred (substitution x n t)   

  | Nil -> Nil
  | Cons (t1, t2) -> Cons (substitution x n t1, substitution x n t2)
  | Head t1 -> Head (substitution x n t1)
  | Tail t1 -> Tail (substitution x n t1)

  | IfZero (cond, t1, t2) -> 
      IfZero (substitution x n cond, substitution x n t1, substitution x n t2)
  | IfEmpty (cond, t1, t2) -> 
      IfEmpty (substitution x n cond, substitution x n t1, substitution x n t2)

  | Fix (phi, m) ->
      if x = phi then t  (* Pas de substitution si la variable est liée *)
      else Fix (phi, substitution x n m)

  | Let (y, t1, t2) ->
      Let (y, substitution x n t1, if x = y then t2 else substitution x n t2)

  | Left t1 -> Left (substitution x n t1)
  | Right t1 -> Right (substitution x n t1)
  | Switch (t1, y, tG, tD) ->
      if x = y then Switch (substitution x n t1, y, tG, tD)
      else Switch (substitution x n t1, y, substitution x n tG, substitution x n tD)
      
  | Prod (t1, t2) -> Prod (substitution x n t1, substitution x n t2)
  | ProjL t1 -> ProjL (substitution x n t1)
  | ProjR t1 -> ProjR (substitution x n t1)

(* Fonction de conversion alpha *)
let rec alphaconv (t : pterm) : pterm =
  match t with
  | Var x -> Var x
  | App (t1, t2) -> App (alphaconv t1, alphaconv t2)
  | Abs (x, t1) ->
      let new_var = nouvelle_var () in
      Abs (new_var, alphaconv (substitution x (Var new_var) t1))
      
  | Int n -> Int n
  | Add (t1, t2) -> Add (alphaconv t1, alphaconv t2)
  | Sub (t1, t2) -> Sub (alphaconv t1, alphaconv t2)
  | Mult (t1, t2) -> Mult (alphaconv t1, alphaconv t2)

  | Succ t -> Succ (alphaconv t)
  | Pred t -> Pred (alphaconv t)

  | Nil -> Nil
  | Cons (t1, t2) -> Cons (alphaconv t1, alphaconv t2)
  | Head t1 -> Head (alphaconv t1)
  | Tail t1 -> Tail (alphaconv t1)

  | IfZero (cond, t1, t2) -> 
      IfZero (alphaconv cond, alphaconv t1, alphaconv t2)
  | IfEmpty (cond, t1, t2) -> 
      IfEmpty (alphaconv cond, alphaconv t1, alphaconv t2)

  | Fix (phi, m) -> 
    let new_phi = nouvelle_var () in
    Fix (new_phi, alphaconv (substitution phi (Var new_phi) m))

  | Let (x, t1, t2) ->
      let new_var = nouvelle_var () in
      Let (new_var, alphaconv t1, alphaconv (substitution x (Var new_var) t2))

  | Left t1 -> Left (alphaconv t1)
  | Right t1 -> Right (alphaconv t1)
  | Switch (t1, x, tG, tD) ->
      let new_var = nouvelle_var () in
      Switch (alphaconv t1, new_var, alphaconv (substitution x (Var new_var) tG), alphaconv (substitution x (Var new_var) tD))
  
  | Prod (t1, t2) -> Prod (alphaconv t1, alphaconv t2)
  | ProjL t1 -> ProjL (alphaconv t1)
  | ProjR t1 -> ProjR (alphaconv t1)

(* Fonction pour vérifier si un terme est une valeur *)
let rec is_value (t : pterm) : bool =
  match t with
  | Var _ | Abs (_, _) | Int _ | Nil -> true
  | Cons (t1, t2) -> is_value t1 && is_value t2
  | Left v | Right v -> is_value v
  | Prod (v1, v2) -> is_value v1 && is_value v2
  | _ -> false

(* Fonction de réduction LtR-CbV *)
(* Fonction de réduction Call-by-Value (LtR-CbV) *)
let rec ltr_cbv_step (t : pterm) : pterm option =
  match t with
  (* Les valeurs ne peuvent pas être réduites *)
  | Var _ | Abs _ | Int _ | Nil -> None  

  (* Application *)
  | App (t1, t2) ->
    (match ltr_cbv_step t1 with
     | Some t1' -> Some (App (t1', t2))  (* Réduction de la partie gauche *)
     | None ->
       match ltr_cbv_step t2 with
       | Some t2' -> Some (App (t1, t2'))  (* Réduction de l'argument *)
       | None ->
         match t1 with
         | Abs (x, body) -> Some (substitution x t2 body)  (* β-réduction *)
         | _ -> None)

  (* Let *)
  | Let (x, e1, e2) ->
    (match ltr_cbv_step e1 with
     | Some e1' -> Some (Let (x, e1', e2))  (* Réduction de e1 *)
     | None -> Some (substitution x e1 e2))  (* Substitution de x par e1 dans e2 *)

  (* Fix *)
  | Fix (phi, m) -> Some (substitution phi (Fix (phi, m)) m)

  (* IfZero *)
  | IfZero (Int 0, t1, _) -> Some t1
  | IfZero (Int _, _, t2) -> Some t2
  | IfZero (t1, t2, t3) ->
    (match ltr_cbv_step t1 with
     | Some t1' -> Some (IfZero (t1', t2, t3))
     | None -> None)

  (* IfEmpty *)
  | IfEmpty (Nil, t1, _) -> Some t1
  | IfEmpty (Cons _, _, t2) -> Some t2
  | IfEmpty (t1, t2, t3) ->
    (match ltr_cbv_step t1 with
     | Some t1' -> Some (IfEmpty (t1', t2, t3))
     | None -> None)

  (* Addition *)
  | Add (Int n1, Int n2) -> Some (Int (n1 + n2))
  | Add (t1, t2) ->
    (match ltr_cbv_step t1 with
     | Some t1' -> Some (Add (t1', t2))  (* Réduction du premier opérande *)
     | None ->
       match ltr_cbv_step t2 with
       | Some t2' -> Some (Add (t1, t2'))  (* Réduction du second opérande *)
       | None -> None)

  (* Soustraction *)
  | Sub (Int n1, Int n2) -> Some (Int (n1 - n2))
  | Sub (t1, t2) ->
    (match ltr_cbv_step t1 with
     | Some t1' -> Some (Sub (t1', t2))  (* Réduction du premier opérande *)
     | None ->
       match ltr_cbv_step t2 with
       | Some t2' -> Some (Sub (t1, t2'))  (* Réduction du second opérande *)
       | None -> None)

  (* Multiplication *)
  | Mult (Int n1, Int n2) -> Some (Int (n1 * n2))
  | Mult (t1, t2) ->
    (match ltr_cbv_step t1 with
     | Some t1' -> Some (Mult (t1', t2))  (* Réduction du premier opérande *)
     | None ->
       match ltr_cbv_step t2 with
       | Some t2' -> Some (Mult (t1, t2'))  (* Réduction du second opérande *)
       | None -> None)

  (* Entier : Successeur *)
  | Succ t ->
    (match ltr_cbv_step t with
      | Some t' -> Some (Succ t')   (* Réduction de l'argument de Succ *)
      | None ->
        match t with
        | Int n -> Some (Int (n + 1))   (* Incrémentation Right'un entier *)
        | _ -> None)

  (* Entier : Predecesseur *)
  | Pred t ->
    (match ltr_cbv_step t with
      | Some t' -> Some (Pred t')   (* Réduction de l'argument de Pred *)
      | None ->
        match t with
        | Int n -> Some (Int (n - 1))   (* Décrémentation Right'un entier *)
        | _ -> None)

  (* Listes : Cons *)
  | Cons (t1, t2) ->
    (match ltr_cbv_step t1 with
     | Some t1' -> Some (Cons (t1', t2))  (* Réduction de la tête *)
     | None ->
       match ltr_cbv_step t2 with
       | Some t2' -> Some (Cons (t1, t2'))  (* Réduction de la queue *)
       | None -> None)

  (* Listes : Head *)
  | Head l ->
    (match l with
     | Cons (hd, _) when is_value hd -> Some hd  (* Extraction de la tête *)
     | _ -> (match ltr_cbv_step l with
             | Some l' -> Some (Head l')  (* Réduction de l'argument de Head *)
             | None -> None))

  (* Listes : Tail *)
  | Tail l ->
    (match l with
     | Cons (_, tl) when is_value tl -> Some tl  (* Extraction de la queue *)
     | _ -> (match ltr_cbv_step l with
             | Some l' -> Some (Tail l')  (* Réduction de l'argument de Tail *)
             | None -> None))

  (* Produits : ProjL *)
  | ProjL (Prod (t1, _)) -> Some t1
  | ProjL t ->
    (match ltr_cbv_step t with
     | Some t' -> Some (ProjL t')  (* Réduction de l'argument *)
     | None -> None)

  (* Produits : ProjR *)
  | ProjR (Prod (_, t2)) -> Some t2
  | ProjR t ->
    (match ltr_cbv_step t with
     | Some t' -> Some (ProjR t')  (* Réduction de l'argument *)
     | None -> None)

  (* Constructeurs de types sommes *)
  | Left t ->
    (match ltr_cbv_step t with
     | Some t' -> Some (Left t')  (* Réduction de l'argument *)
     | None -> None)
  | Right t ->
    (match ltr_cbv_step t with
     | Some t' -> Some (Right t')  (* Réduction de l'argument *)
     | None -> None)

  (* Types Sommes : Switch *)
  | Switch (t, x, t1, t2) ->
    (match ltr_cbv_step t with
     | Some t' -> Some (Switch (t', x, t1, t2))  (* Réduction de l'argument *)
     | None ->
       match t with
       | Left v when is_value v -> Some (substitution x v t1)  (* Remplacer x par v dans t1 *)
       | Right v when is_value v -> Some (substitution x v t2)  (* Remplacer x par v dans t2 *)
       | _ -> None)

  (* Cas par défaut : Pas de réduction possible *)
  | _ -> None

(* Fonction de normalisation *)
let rec ltr_cbv_norm (t : pterm) : pterm =
  match ltr_cbv_step t with
  | Some t' -> ltr_cbv_norm t'
  | None -> t

(* Fonction de normalisation avec une limite *)
let rec ltr_cbv_norm_with_limit (t : pterm) (limit : int) : pterm =
  if limit <= 0 then raise Timeout
  else
    match ltr_cbv_step t with
    | Some t' -> ltr_cbv_norm_with_limit t' (limit - 1)
    | None -> t