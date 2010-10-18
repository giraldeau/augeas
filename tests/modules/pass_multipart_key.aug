module Pass_multipart_key = 

    (* Multipart key adds a static label to a key in the get direction       *)
    (* and removes it in the put direction. The main purpose is to avoid put *) 
    (* ambiguity for similar keys.                                           *)

(* 
    let k1 = [ label "a" . key /[b]*/ ]
    test k1 get "b" = { "ab" }
    test k1 put "b" after rm "x" = "b"
  
    let k2 = [ label "a" . [ key /[c]+/ ] . key /[b]+/ ]
    test k2 get "cb" = { "ab" { "c" } }
    test k2 put "cb" after rm "x" = "cb"
    
    let k3 = [ label "a" . key /[b]+/ . del "c" "c" ] *
    test k3 get "bcbbc" = { "ab" } { "abb" }
    test k3 put "bc" after clear "/abb" = "bcbbc"

    let rec k4 = [ label "a" . key /[b]+/ . del "c" "c" ] . k4?
    test k4 get "bcbbc" = { "ab" } { "abb" }
    test k4 put "bc" after clear "/abb" = "bcbbc"
*)
    let rec k5 = [ label "a" . key "b" . k5? . del "c" "c" ]
    test k5 get "bbcc" = { "ab" { "ab" } }
    (* test k5 put "bc" after clear "/ab/ab" = "bcbc" *)