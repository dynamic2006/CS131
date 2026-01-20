let my_subset_test0 = subset ["hello"; "ciao"; "hello"] ["hello"; "ciao"] 
let my_subset_test1 = not (subset ["hello"; "ciao"] [])
let my_subset_test2 = subset [] ["hello"; "ciao"]
let my_subset_test3 = subset [] []
let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2;3;2;1;1] [3;1;2]
let my_equal_sets_test2 = equal_sets [3;1;2] [1;2;3;2;1;1]
let my_set_union_test0 = equal_sets (set_union [] ["this"; "better"; "work"]) ["this"; "better"; "work"]
let my_set_union_test1 = equal_sets (set_union ["this"; "better"; "work"] []) ["this"; "better"; "work"]
let my_set_union_test2 = equal_sets (set_union [] []) []
let my_set_all_union_test0 = equal_sets (set_all_union [[]; []; []]) []
let my_set_all_union_test1 = equal_sets (set_all_union [["ab"; "a"]; ["ba"; "ab"]; ["a"; "b"; "c"]]) ["ab"; "ba"; "a"; "b"; "c"]
let my_self_member_test0 = not (self_member []) (* Note that the set containing the emptyset is NOT the emptyset! *)
let my_computed_fixed_point_test0 = 
  computed_fixed_point (equal_sets) (fun s -> 1 :: s) [2;3] = [1;2;3]
let my_computed_periodic_point_test0 =
  (computed_periodic_point (=) (fun x -> x * x) 2 (-1)) = 1
let my_whileseq_test0 =
  whileseq (fun s -> 2 :: s) (fun s -> not (subset [1;2] s)) [1] = [[1]]

(* JUJU GRAMMAR [FT. EGGERT] *)

type juju_nonterminals =
  | JUJU | EGGERT | NUM | OP

let juju_rules =
   [JUJU, [T"("; N JUJU; T")"];
    JUJU, [N EGGERT];
    JUJU, [N NUM; N OP; N NUM];
    EGGERT, [N EGGERT];
    OP, [T"+"];
    OP, [T"-"];
    NUM, [T"6"];
    NUM, [T"7"];]

let juju_grammar = JUJU, juju_rules

let my_filter_blind_alleys_test0 =
  filter_blind_alleys juju_grammar =
  (JUJU, 
  [
    JUJU, [T"("; N JUJU; T")"];
    JUJU, [N NUM; N OP; N NUM];
    OP, [T"+"];
    OP, [T"-"];
    NUM, [T"6"];
    NUM, [T"7"];
  ])

