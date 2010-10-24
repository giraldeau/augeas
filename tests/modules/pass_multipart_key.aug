module Pass_multipart_key =

  (* check multi key concat *)
  let l1 = key /a/ . key /b/ 
  let l2 = ( key /a/ ) *
  let rec l3 = [ key "a" . l3? ]

  (* union typecheck *)
  let l4 = ([ key "a"  ] . del "y" "y" ) | del "x" "x"

