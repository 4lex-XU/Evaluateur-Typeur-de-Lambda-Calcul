(* --------------- Types pour le λ-calcul --------------- *)
open Eval
exception Timeout

(* --------------- AST TYPES ---------------*)
type ptype =  Var of string             (* Variable de type *)
            | Arr of ptype * ptype      (* Type flèche *)
            | Nat                       (* Type Nat *)
            | List of ptype             (* Type Liste *)
            | ForAll of string * ptype  (* Type universel *)
            | Sum of ptype * ptype      (* Type somme *)
            | Prod of ptype * ptype     (* Type produit *)

(* Pretty-printer pour les types *)
let rec print_type (t : ptype) : string =
    match t with
    | Var x -> x
    | Arr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
    | Nat -> "Nat"
    | List t -> "List(" ^ (print_type t) ^ ")"
    | ForAll (x, t) -> "ForAll : " ^ x ^ "." ^ (print_type t)
    | Sum (t1, t2) -> "(" ^ (print_type t1) ^ " + " ^ (print_type t2) ^ ")"
    | Prod (t1, t2) -> "(" ^ (print_type t1) ^ " * " ^ (print_type t2) ^ ")"

let compteur_var_t : int ref = ref 0

let nouvelle_var_t () : string = compteur_var_t := !compteur_var_t + 1;
  "T"^( string_of_int !compteur_var_t )

type equa = (ptype * ptype) list

type env = (string * ptype) list

(* substitue une variable de type par un type à l'intérieur Right'un autre type *)
let rec substitution_t (x : string) (t : ptype) (ty : ptype) : ptype =
  match ty with
  | Var y -> if x = y then t else Var y
  | Arr (t1, t2) -> Arr (substitution_t x t t1, substitution_t x t t2)
  | Nat -> Nat
  | List t1 -> List (substitution_t x t t1)
  | ForAll (y, t1) -> if x = y then ForAll (y, t1) else ForAll (y, substitution_t x t t1)
  | Prod (t1, t2) -> Prod (substitution_t x t t1, substitution_t x t t2)
  | Sum (t1, t2) -> Sum (substitution_t x t t1, substitution_t x t t2)

(* substitue une variable de type par un type partout dans un système Right'équation *)
let rec substitution_equa (x : string) (t : ptype) (eq : equa) : equa =
  match eq with
  | [] -> []
  | (t1, t2)::q -> (substitution_t x t t1, substitution_t x t t2)::(substitution_equa x t q)

(* Fonction Right'instanciation Right'un type *)
let rec instancier (t : ptype) : ptype =
  match t with
  | ForAll (x, t1) -> 
      let new_var_t = Var (nouvelle_var_t ()) in
      substitution_t x new_var_t (instancier t1)
  | _ -> t

let rec cherche_type (v : string) (e : env) : ptype =
  match e with
  | [] -> failwith "Variable non trouvée"
  | (x, t)::rest -> 
      if x = v then instancier t
      else cherche_type v rest

(* Fonction de généralisation Right'un type à partir de l'environnement *)
let generaliser (ty : ptype) (env : env) : ptype =
  let rec variables_libres (ty : ptype) : string list =
    match ty with
    | Var x -> [x]
    | Arr (t1, t2) -> (variables_libres t1) @ (variables_libres t2)
    | Nat -> []
    | List t -> variables_libres t
    | ForAll (x, t) -> List.filter (fun v -> v <> x) (variables_libres t)
    | Prod (t1, t2) -> (variables_libres t1) @ (variables_libres t2)
    | Sum (t1, t2) -> (variables_libres t1) @ (variables_libres t2)
  in 
  let env_vars = List.concat_map (fun (_, t) -> variables_libres t) env in
  let unique lst = List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) [] lst in
  let libres = List.filter (fun v -> not (List.mem v env_vars)) (variables_libres ty) |> unique in
  let res = List.fold_right (fun x t -> ForAll (x, t)) libres ty in res

let rec contient_type target_ty t =
  t = target_ty ||
  match t with
  | Arr(t1, t2) -> contient_type target_ty t1 || contient_type target_ty t2
  | _ -> false

let with_timeout duration f x =
  let start_time = Sys.time () in
  let rec loop () =
    if Sys.time () -. start_time > duration then 
      raise Timeout
    else
      try f x with
      | Timeout -> raise Timeout
      | _ -> loop ()
  in
  loop ()

(* fonction occur check qui vérifie si une variable appartient à un type. *)
let rec occur_check (x : string) (t : ptype) : bool =
  match t with
  | Var y -> x = y
  | Arr (t1, t2) -> (occur_check x t1) || (occur_check x t2)
  | Nat -> false
  | List t -> occur_check x t
  | ForAll (y, t) -> if x = y then false else occur_check x t
  | Prod (t1, t2) -> (occur_check x t1) || (occur_check x t2)
  | Sum (t1, t2) -> (occur_check x t1) || (occur_check x t2)

(* renomme les variables de type dans un type *)
let rec rename_vars (t : ptype) (renaming : (string * string) list) : ptype =
  match t with
  | Var x -> 
      (match List.assoc_opt x renaming with
        | Some new_x -> Var new_x
        | None -> Var x)
  | Arr (t1, t2) -> Arr (rename_vars t1 renaming, rename_vars t2 renaming)
  | Nat -> Nat
  | List t1 -> List (rename_vars t1 renaming)
  | ForAll (x, t1) -> 
      let new_x = nouvelle_var_t () in
      ForAll (new_x, rename_vars t1 ((x, new_x)::renaming))
  | Prod (t1, t2) -> Prod (rename_vars t1 renaming, rename_vars t2 renaming)
  | Sum (t1, t2) -> Sum (rename_vars t1 renaming, rename_vars t2 renaming)


(* Une étape Right'unification *)
let rec unif_step (eqs : equa) (substitutions_acc : env) : (equa * env) option =
  match eqs with
  | [] -> Some ([], substitutions_acc)
  
  | (t1, t2)::rest when t1 = t2 -> 
    unif_step rest substitutions_acc 
        
  | (Var x, t2)::rest when not (occur_check x t2) -> 
    let new_eqs = substitution_equa x t2 rest in
    let new_substitutions = (x, t2) :: substitutions_acc in
    Some (new_eqs, new_substitutions)
  
  | (t1, Var x)::rest when not (occur_check x t1) -> 
    let new_eqs = substitution_equa x t1 rest in
    let new_substitutions = (x, t1) :: substitutions_acc in
    Some (new_eqs, new_substitutions)

  | (Arr (t1a, t1b), Arr (t2a, t2b))::rest ->
    Some ((t1a, t2a)::(t1b, t2b)::rest, substitutions_acc)

  | (List t1, List t2)::rest ->
    Some ((t1, t2)::rest, substitutions_acc)

  | (ForAll (x, t1), t2)::rest ->
    let t1_renamed = rename_vars t1 [] in
    let t1_opened = substitution_t x t1_renamed t2 in
    Some ((t1_opened, t2)::rest, substitutions_acc)

  | (t1, ForAll (x, t2))::rest ->
    let t2_renamed = rename_vars t2 [] in
    let t2_opened = substitution_t x t2_renamed t1 in
    Some ((t1, t2_opened)::rest, substitutions_acc)
  
  | (Prod (t1a, t1b), Prod (t2a, t2b))::rest ->
    Some ((t1a, t2a)::(t1b, t2b)::rest, substitutions_acc)
  | (Sum (t1a, t1b), Sum (t2a, t2b))::rest ->
    Some ((t1a, t2a)::(t1b, t2b)::rest, substitutions_acc)

  | _ -> None


let rec unif (eqs : equa) (substitutions_acc : env) : (equa * env) option =

  match unif_step eqs substitutions_acc with
  | None -> None
  | Some ([], new_substitutions) -> Some ([], new_substitutions)
  | Some (new_eqs, new_substitutions) -> 
      unif new_eqs new_substitutions

let solve_equations_with_timeout (eqs : equa) (timeout_duration : float) : (equa * env) option =
  try
    with_timeout timeout_duration (fun substitutions_acc -> unif eqs substitutions_acc) []
  with
  | Timeout -> None

(* Substitution de type *)
let rec apply_substitutions (t : ptype) (substitutions : env) : ptype =
  match t with
  | Var x -> 
      (match List.assoc_opt x substitutions with
        | Some t' -> t'
        | None -> t)
  | Arr (t1, t2) -> 
      Arr ( apply_substitutions t1 substitutions, 
            apply_substitutions t2 substitutions )
  | List t1 -> List (apply_substitutions t1 substitutions)
  | ForAll (x, t1) -> ForAll (x, apply_substitutions t1 substitutions)
  | Nat -> Nat
  | Prod (t1, t2) -> Prod (apply_substitutions t1 substitutions, apply_substitutions t2 substitutions)
  | Sum (t1, t2) -> Sum (apply_substitutions t1 substitutions, apply_substitutions t2 substitutions)

(* vérifie si un type est bien formé *)
let rec check_type (t : ptype) : bool =
  match t with
  | Var _ -> true
  | Arr (t1, t2) -> (check_type t1) && (check_type t2)
  | Nat -> true
  | List t1 -> check_type t1
  | ForAll (_, t1) -> check_type t1
  | Prod (t1, t2) -> (check_type t1) && (check_type t2)
  | Sum (t1, t2) -> (check_type t1) && (check_type t2)

let rec genere_equa (te : pterm) (ty : ptype) (e : env)  : equa =
  match te with
  | Var v ->
      let tv = cherche_type v e in
      [(tv, ty)]
  | App (t1, t2) ->
      let ta = Var(nouvelle_var_t()) in 
      let eq1 = genere_equa t1 (Arr(ta, ty)) e in 
      let eq2 = genere_equa t2 ta e in
      eq1 @ eq2
  | Abs (x, t) ->
      let ta = Var(nouvelle_var_t()) in
      let tr = Var(nouvelle_var_t()) in
      let eq1 = [(ty, Arr(ta, tr))] in
      let eq2 = genere_equa t tr ((x, ta)::e) in
      eq1 @ eq2
  
  (* Entiers *)
  | Int _ ->
    [(ty, Nat)]

  | Add (t1, t2) | Sub (t1, t2) | Mult (t1, t2) ->
    let ta = Var (nouvelle_var_t ()) in                            
    let tr = Var (nouvelle_var_t ()) in                            
    let eq1 = genere_equa t1 ta e in
    let eq2 = genere_equa t2 tr e in
    (ty,  Nat) :: eq1 @ eq2

  | IfZero (cond, then_branch, else_branch) ->
    let eq1 = genere_equa cond Nat e in
    let eq2 = genere_equa then_branch ty e in
    let eq3 = genere_equa else_branch ty e in
    eq1 @ eq2 @ eq3

  | Succ t1 | Pred t1 ->
    let eq1 = genere_equa t1 Nat e in
    [(ty, Nat)] @ eq1

  (* Listes *)
  | Nil ->
    let x = Var (nouvelle_var_t ()) in
    [(ty, List x)]

  | Cons (t1, t2) ->
    let x = Var (nouvelle_var_t ()) in
    let eq1 = genere_equa t1 x e in
    let eq2 = genere_equa t2 (List x) e in
    let eq3 = [(ty, List x)] in
    eq1 @ eq2 @ eq3
  
  | Head t1 ->
    let x = Var (nouvelle_var_t ()) in
    let eq1 = genere_equa t1 (List x) e in
    let eq2 = [(ty, x)] in
    eq1 @ eq2

  | Tail t1 ->
    let x = Var (nouvelle_var_t ()) in
    let eq1 = genere_equa t1 (List x) e in
    let eq2 = [(ty, List x)] in
    eq1 @ eq2

  | IfEmpty (cond, then_branch, else_branch) ->
    let eq1 = genere_equa cond (List (Var (nouvelle_var_t ())) ) e in
    let eq2 = genere_equa then_branch ty e in
    let eq3 = genere_equa else_branch ty e in
    eq1 @ eq2 @ eq3

  (* Point fixe *)
  | Fix (x, body) ->
    let t1 = Var (nouvelle_var_t ()) in
    let t2 = Var (nouvelle_var_t ()) in
    let eq1 = (ty, Arr (t1, t2)) in
    let e_new = (x, Arr (t1, t2))::e in
    let eq2 = genere_equa body (Arr (t1, t2)) e_new in
    eq1 :: eq2  
    
  (* Let *)
  | Let (x, e1, e2) ->
    let t0 = infer_type e1 e in
    let t0_gen = match t0 with
      | Some t -> generaliser t e
      | None -> failwith "Type non inférable" in
    let new_env = (x, t0_gen)::e in
    genere_equa e2 ty new_env

  (* Type Produit *)
  | Prod (e1, e2) ->
    let t1 = Var (nouvelle_var_t ()) in  
    let t2 = Var (nouvelle_var_t ()) in  
    let eq1 = genere_equa e1 t1 e in  
    let eq2 = genere_equa e2 t2 e in  
    let eq3 = [(ty, Prod (t1, t2))] in
    eq1 @ eq2 @ eq3
  | ProjL e1 ->
    let t1 = Var (nouvelle_var_t ()) in  
    let t2 = Var (nouvelle_var_t ()) in  
    let eq1 = genere_equa e1 (Prod (t1, t2)) e in                 
    [(ty, t1)] @ eq1

  | ProjR e1 ->
    let t1 = Var (nouvelle_var_t ()) in  
    let t2 = Var (nouvelle_var_t ()) in  
    let eq1 = genere_equa e1 (Prod (t1, t2)) e in  
    [(ty, t2)] @ eq1
  (* Type Somme *)
  | Switch (e1, x, eg, ed) ->
    let t2 = Var (nouvelle_var_t ()) in  
    let t3 = Var (nouvelle_var_t ()) in  
    let eq1 = genere_equa e1 (Sum (t2, t3)) e in  
    let eq2 = genere_equa eg ty ((x, t2)::e) in  
    let eq3 = genere_equa ed ty ((x, t3)::e) in  
    eq1 @ eq2 @ eq3
  | Left e1 ->
    let t1 = Var (nouvelle_var_t ()) in 
    let t2 = Var (nouvelle_var_t ()) in  
    let eq1 = genere_equa e1 t1 e in  
    eq1 @ [(ty, Sum(t1,t2))]  
  | Right e1 ->
    let t1 = Var (nouvelle_var_t ()) in  
    let t2 = Var (nouvelle_var_t ()) in  
    let eq1 = genere_equa e1 t2 e in  
    eq1 @ [(ty, Sum(t1,t2))] 

and infer_type (te : pterm) (env : env) : ptype option =
  let ty = Var (nouvelle_var_t ()) in
  let equations = genere_equa te ty env in
  let (eqs_with_ty, eqs_without_ty) =
    List.partition (fun (t1, t2) ->
      contient_type ty t1 || contient_type ty t2) equations in
  let new_eqs = eqs_without_ty @ eqs_with_ty in
  match solve_equations_with_timeout new_eqs 2.0 with
  | None -> None
  | Some (eqs, substitutions) -> 
    let final_type = apply_substitutions ty substitutions in
    Some final_type