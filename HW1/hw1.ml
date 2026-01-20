let rec subset a b = 
  match a with
  | [] -> true
  | head :: tail -> List.mem head b && subset tail b
;;

let equal_sets a b = subset a b && subset b a;;

let rec set_union a b =
  match (a,b) with
  | ([], []) -> []
  | (_ , head :: tail) -> 
    (
      match List.mem head (a@tail) with
    | true -> set_union a tail
    | false -> head :: set_union a tail
    )
  | (head :: tail, []) ->
    (
      match List.mem head tail with
      | true -> set_union tail []
      | false -> head :: set_union tail []
    )
;;

let rec set_all_union a =
  match a with
  | [] -> []
  | head :: tail -> set_union head (set_all_union tail)
;;

let self_member s = false;;
(* In OCaml, any set represented by some list s has type 'a list with elements of type 'a.
Since the input s is "discrete" (has to be written in Ocaml), s can never be a member of itself because type 'a list is never the same as type 'a in Ocaml static type checking.
So a simple way to write the function is to always return false. On the other hand, no meaningful self_member function can be written precisely because Ocaml has static type checking,
so trying to write List.mem s s or some variation of it will lead to the same 'a list vs 'a type compile-time issue.*)

let rec computed_fixed_point eq f x = 
  match eq (f x) x with
  | true -> x
  | false -> computed_fixed_point eq f (f x)
;;

let rec compute_composition f p x = 
  match p with
  | 0 -> x
  | _ -> compute_composition f (p-1) (f x)
;;

let rec computed_periodic_point eq f p x =
  match eq (compute_composition f p x) x with
  | true -> x
  | false -> computed_periodic_point eq f p (f x)
;;

let rec whileseq s p x = 
  match p x with
  | false -> []
  | true -> x :: whileseq s p (s x)
;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec is_productive rhs productive_list = 
  match rhs with
  | [] -> true
  | head :: tail ->
    (
      match head with
      | T _ -> is_productive tail productive_list
      | N sym -> (List.mem sym productive_list) && (is_productive tail productive_list)
    )
;;
(* checks if the rhs for a rule is productive *)

let update_rule lhs rhs productive_list = 
  match is_productive rhs productive_list with
  | true -> set_union [lhs] productive_list
  | false -> productive_list
(* updates productive list given a rule (lhs, rhs) with symbol lhs if rule is productive *)

let rec update_rules rules productive_list = 
  match rules with
  | [] -> productive_list
  | head :: tail ->
    (
      match head with
      | (lhs, rhs) -> update_rules tail (update_rule lhs rhs productive_list)
    )
;;
(* scans entire list of rules and updates productive list for each rule *)

let get_final_prod rules = computed_fixed_point equal_sets (fun prod -> update_rules rules prod) []
(* computes fixed point of the productive symbols in the grammar -- ie when no more symbols are productive, final list is achieved *)

let rec get_filtered_rules rules final_prod = 
  match rules with
  | [] -> []
  | head :: tail ->
    (
      match head with
      | (lhs, rhs) -> 
        (
          match (List.mem lhs final_prod) && (is_productive rhs final_prod) with
          | true -> head :: (get_filtered_rules tail final_prod)
          | false -> get_filtered_rules tail final_prod
        )
    )
;;
(* helper function to filter out non-productive rules [either lhs symbol is not productive or rhs rule is not productive] *)
let filter_blind_alleys g =
  match g with
  | (start_symbol, rules) -> (start_symbol, get_filtered_rules rules (get_final_prod rules))
;;
(* final function *)

