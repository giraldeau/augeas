(* BIND dhcp 3 server configuration module for Augeas 
   Author: Francis Giraldeau <francis.giraldeau@...>

   Reference: man dhcpd.conf
   Follow dhclient module for tree structure
*)

module Dhcpd = 

autoload xfm

(************************************************************************
 *                           USEFUL PRIMITIVES
 *************************************************************************)

let eol               = Util.eol
let comment           = Util.comment
let empty             = Util.empty
let indent            = Util.indent
let eos               = comment?

(* Define separators *)
let sep_spc           = del /[ \t]+/ " "
let sep_scl           = del /[ \t]*;/ ";"
let sep_obr           = del /[ \t]*\{/ " {"
let sep_cbr           = del /[ \t]*\}([ \t]*\n)*/ " }\n"
let sep_com           = del /[ \t\n]*,[ \t\n]*/ ","
let sep_slh           = del "\/" "/"
let sep_col           = del ":" ":"
let sep_eq            = del /[ \t]*=[ \t]*/ "="
let scl               = del ";" ";"

(* Define basic types *)
let word              = /[A-Za-z0-9_.-]+(\[[0-9]+\])?/
let ip                = Rx.ipv4

(* Define fields *)

(* TODO: there could be a " " in the middle of a value ... *)
let sto_to_spc        = store /[^\\#,;\{\}" \t\n]+|"[^\\#"\n]+"/
let sto_to_scl        = store /[^ \t;][^;\n=]+[^ \t;]|[^ \t;=]+/
let rfc_code          = [ key "code" . sep_spc . store word ]
                      . sep_eq
                      . [ label "value" . sto_to_scl ]

let sto_number        = store /[0-9][0-9]*/

(************************************************************************
 *                         NO ARG STATEMENTS
 *************************************************************************)

let stmt_noarg_re     = "authoritative"

let stmt_noarg        = [ indent
                        . key stmt_noarg_re 
                        . sep_scl
                        . eos ]

(************************************************************************
 *                         INT ARG STATEMENTS
 *************************************************************************)

let stmt_integer_re   = "default-lease-time"
                      | "max-lease-time"
                      | "min-lease-time"
                      | "lease limit"

let stmt_integer      = [ indent
                        . key stmt_integer_re
                        . sep_spc
                        . sto_number
                        . sep_scl 
                        . eos ]

(************************************************************************
 *                         STRING ARG STATEMENTS
 *************************************************************************)

let stmt_string_re    = "ddns-update-style"
                      | "ddns-updates"
                      | "log-facility" 
                      | "filename" 
                      | "server-name" 
                      | "fixed-address"
                      | "failover peer"
                      | "use-host-decl-names"

let stmt_string       = [ indent  
                        . key stmt_string_re 
                        . sep_spc 
                        . sto_to_spc
                        . sep_scl
                        . eos ] 

(************************************************************************
 *                         RANGE STATEMENTS
 *************************************************************************)

let stmt_range        = [ indent 
                        . key "range" 
                        . sep_spc
                        . [ label "flag" . store /dynamic-bootp/ . sep_spc ]?
                        . [ label "from" . store ip . sep_spc ]?
                        . [ label "to" . store ip ]
                        . sep_scl
                        . eos ]

(************************************************************************
 *                         HARDWARE STATEMENTS
 *************************************************************************)

let stmt_hardware     = [ indent
                        . key "hardware"
                        . sep_spc  
                        . [ label "type" . store /ethernet|tokenring/ ] 
                        . sep_spc 
                        . [ label "address" . store /[a-fA-F0-9:-]+/ ] 
                        . sep_scl
                        . eos ]

(************************************************************************
 *                         OPTION STATEMENTS
 *************************************************************************)
(* We do not parse further the option's value, because custom format can
   be defined at runtime *)
(* FIXME: handle multi value ---> remove "=" in sto_to_spc *)

let stmt_option_re    = "option"

let stmt_option       = [ indent 
                        . key stmt_option_re
                        . sep_spc
                        (* . [ key word . sep_spc . (sto_to_scl|rfc_code) ] *) 
                        . [ key word . sep_spc . (sto_to_scl|rfc_code) ] 
                        . sep_scl
                        . eos ]

(************************************************************************
 *                         ALLOW/DENY STATEMENTS
 *************************************************************************)
(* We have to use special key for allow/deny members of
  to avoid ambiguity in the put direction *)

let allow_deny_re     = "unknown-clients"
                      | "dynamic bootp clients"
                      | "authenticated clients"
                      | "unauthenticated clients"

let stmt_secu_re      = "allow"
                      | "deny"

let stmt_secu         = [ indent
                        . key stmt_secu_re 
                        . sep_spc 
                        . store allow_deny_re 
                        . sep_scl
                        . eos ]
                      | [ indent 
                        . del /allow[ \t]+members[ \t]+of/ "allow members of"
                        . sep_spc 
                        . label "allow-members-of" 
                        . sto_to_spc 
                        . sep_scl
                        . eos ]
                      | [ indent 
                        . del /deny[ \t]+members[ \t]+of/ "deny members of" 
                        . sep_spc 
                        . label "deny-members-of" 
                        . sto_to_spc 
                        . sep_scl
                        . eos ]

(************************************************************************
 *                         BLOCK STATEMENTS
 *************************************************************************)
(* Blocks doesn't support comments at the end of the closing bracket *)

let stmt_entry        =   stmt_secu 
                        | stmt_option
                        | stmt_hardware
                        | stmt_range
                        | stmt_string
                        | stmt_integer
                        | stmt_noarg 
                        | empty
                        | comment

let stmt_block_noarg_re = "pool"
                        | "group"

let stmt_block_noarg (body:lens)
                        = [ indent 
                        . key stmt_block_noarg_re
                        . sep_obr 
                        . body*
                        . sep_cbr ]

let stmt_block_arg_re = "host"
                      | "class"
                      | "shared-network"

let stmt_block_arg (body:lens)  
                      = [ indent 
                        . key stmt_block_arg_re
                        . sep_spc
                        . sto_to_spc
                        . sep_obr
                        . body*
                        . sep_cbr ]

let stmt_block_subnet (body:lens) 
                      = [ indent 
                        . key "subnet" 
                        . sep_spc 
                        . [ label "network" . store ip ] 
                        . sep_spc 
                        . [ key "netmask" . sep_spc . store ip ] 
                        . sep_obr
                        . body*
                        . sep_cbr ]

let all_block (body:lens) = 
    let lns1 = stmt_block_subnet body in
    let lns2 = stmt_block_arg body in
    let lns3 = stmt_block_noarg body in
    (lns1 | lns2 | lns3 | stmt_entry)

let rec lns_staging = stmt_entry|all_block lns_staging
let lns = (lns_staging)*

let xfm = transform lns (incl "/etc/dhcp3/dhcpd.conf")
