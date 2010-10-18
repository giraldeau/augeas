module Fail_multipart_ambig = 

    let k1 = [ label "a" . key /[b]*/ ] | [ key /(a)[b]*/ ]
