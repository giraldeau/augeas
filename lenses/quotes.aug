module Quotes =

let dels (s:string) = del s s

let q1 = dels "'"
let q2 = dels "\""
let q3 = del /['\"]/ "\""

let att1 = key /[^'\/]*[\"][^'\/]*/
let att2 = key /[^\"\/]*['][^\"\/]*/
let att3 = key /[^'\"\/]*/

let sto_q1 =
    let quote = dels "'" in
    let body = store /[^']*[\"][^']*/
    square quote body quote

let sto_q2 =
    let quote = dels "\"" in
    let body = store /[^\"]*['][^\"]*/
    square quote body quote

let sto_q3 =
    let quote = del /['\"]/ "\"" in
    let body = store /[^'\"]*/
    square quote body quote

let att_value =
[ square del1 att1 del1 ] |
[ square del2 att2 del2 ] |
[ square del3 att3 del3 ]

(* Put ambiguity with empty string is raised
   because atype is not complete at the
   typecheck time on the union, must have a
   complete subtree
let att_value =
[ square del1 att1 del1 |
  square del2 att2 del2 |
  square del3 att3 del3 ]
*)

(* well formed values *)
test att_value get "\"value\"" = { "value" }
test att_value get "'value'" = { "value" }
test att_value get "'va\"lue'" = { "va\"lue" }
test att_value get "\"va'lue\"" = { "va'lue" }

(* illegal as per the XML standard *)
test att_value get "\"va\"lue\"" = *
test att_value get "'va'lue'" = *

(* malformed values *)
test att_value get "\"value'" = *
test att_value get "'value\"" = *


