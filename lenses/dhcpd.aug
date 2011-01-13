(* 
Module: Dhcpd
  BIND dhcp 3 server configuration module for Augeas 
  
Author: Francis Giraldeau <francis.giraldeau@...>

About: Reference  
  Reference: manual of dhcpd.conf and dhcp-eval 
  Follow dhclient module for tree structure

About: License
    This file is licensed under the GPL.

About: Lens Usage
  TODO

About: Configuration files
  This lens applies to /etc/dhcpd3/dhcpd.conf. See <filter>.
*)

module Dhcpd = 

autoload xfm

(************************************************************************
 *                           USEFUL PRIMITIVES
 *************************************************************************)
let dels (s:string)   = del s s
let eol               = Util.eol
let comment           = Util.comment
let empty             = Util.empty
let indent            = Util.indent
let eos               = comment?

(* Define separators *)
let sep_spc           = del /[ \t]+/ " "
let sep_osp           = del /[ \t]*/ ""
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

(* borrowed from sysconfig.aug *)
  (* Chars allowed in a bare string *)
  let bchar = /[^ \t\n\"'\\{\}#,\(\)]|\\\\./
  let qchar = /["']/  (* " *)

  (* We split the handling of right hand sides into a few cases:
   *   bare  - strings that contain no spaces, optionally enclosed in
   *           single or double quotes
   *   dquot - strings that contain at least one space or apostrophe,
   *           which must be enclosed in double quotes
   *   squot - strings that contain an unescaped double quote
   *)
  let bare = del qchar? "" . store (bchar+) . del qchar? ""
  let dquot =
    del qchar "\"" . store (bchar* . /[ \t']/ . bchar*)+ . del qchar "\""
  let squot =
    dels "'" . store ((bchar|/[ \t]/)* . "\"" . (bchar|/[ \t]/)*)+ . dels "'"

let sto_to_spc        = store /[^\\#,;\{\}" \t\n]+|"[^\\#"\n]+"/
let sto_to_scl        = store /[^ \t;][^;\n=]+[^ \t;]|[^ \t;=]+/

let sto_number        = store /[0-9][0-9]*/

(************************************************************************
 *                         NO ARG STATEMENTS
 *************************************************************************)

let stmt_noarg_re     =   "authoritative" 
                        | "primary"
                        | "secondary"

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
                      | "port"
                      | /peer[ ]+port/
                      | "max-response-delay"
                      | "max-unacked-updates"
                      | "mclt"
                      | "split"
                      | /load[ ]+balance[ ]+max[ ]+seconds/
                      | "max-lease-misbalance"
                      | "max-lease-ownership"
                      | "min-balance"
                      | "max-balance"

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
                      | /failover[ ]+peer/
                      | "use-host-decl-names"
                      | "next-server"
                      | "address"
                      | /peer[ ]+address/
                      | "type"
                      | "file"
                      | "algorithm"
                      | "secret"
                      | "key"
                      | "include"
                      | "hba"

let stmt_string_tpl (l:lens) = [ indent  
                        . key stmt_string_re 
                        . sep_spc
                        . l
                        . sep_scl
                        . eos ]
                        
let stmt_string  = stmt_string_tpl bare |stmt_string_tpl squot | stmt_string_tpl dquot   

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
(* The general case is considering options as a list *)

let stmt_option_code  = [ label "label" . store word . sep_spc ]
                        . [ key "code" . sep_spc . store word ]
                        . sep_eq
                        . [ label "type" . store word ] 


let stmt_option_list  = counter "id"
                        . ([ seq "id" . bare ] | [ seq "id" . dquot ] | [ seq "id" . squot ])
                        . ( sep_com . ([ seq "id" . bare ] | [ seq "id" . dquot ] | [ seq "id" . squot ]))* 

let stmt_option_basic = [ key word . sep_spc . stmt_option_list ]
let stmt_option_extra = [ key word . sep_spc . store /true|false/ . sep_spc . stmt_option_list ] 

let stmt_option_body = stmt_option_basic | stmt_option_extra  

let stmt_option1  = [ indent 
                        . key "option"
                        . sep_spc
                        . stmt_option_body
                        . sep_scl
                        . eos ]

let stmt_option2  = [ indent 
                        . dels "option" . label "rfc-code"
                        . sep_spc
                        . stmt_option_code
                        . sep_scl
                        . eos ]

let stmt_option = stmt_option1 | stmt_option2

(************************************************************************
 *                         SUBCLASS STATEMENTS
 *************************************************************************)
(* this statement is not well documented in the manual dhcpd.conf
   we support basic use case *)

let stmt_subclass = [ indent . key "subclass" . sep_spc . 
                      ([ label "name" . dquot ]|
                       [ label "name" . squot ]|
                       [ label "name" . bare ]) . sep_spc .
                       [ label "value" . bare ] . sep_scl . eos ]

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
 *                         MATCH STATEMENTS
 *************************************************************************)

let sto_fct = store (word . /[ \t]*\([^)]*\)/)
let sto_option = store (/option[ ]+/ . word)
let sto_com = /[^ \t\n,\(\)][^,\(\)]*[^ \t\n,\(\)]|[^ \t\n,\(\)]+/ | word . /[ \t]*\([^)]*\)/ 
let fct_re = "substring" | "binary-to-ascii"

let fct_args = [ label "args" . dels "(" . counter "args" . sep_osp .
                 ([ seq "args" . store sto_com ] . [ seq "args" . sep_com . store sto_com ]+) . 
                        sep_osp . dels ")" ]

let stmt_match_if = [ dels "if" . sep_spc . store fct_re . sep_osp . label "function" . fct_args ] . 
                      sep_eq . ([ label "value" . bare ]|[ label "value" . squot ]|[ label "value" . dquot ]) 

let stmt_match_pfv = [ label "function" . store "pick-first-value" . sep_spc .
                       dels "(" . sep_osp . 
                       [ counter "opt" . label "args" .
                         [ seq "opt" . store sto_com ] . 
                         [ sep_com . seq "opt" . store sto_com ]+ ] . 
                       dels ")" ]
                              
let stmt_match_tpl (l:lens) = [ indent . key "match" . sep_spc . l . sep_scl . eos ]

let stmt_match = stmt_match_tpl (stmt_match_if | stmt_match_pfv )

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
                        | stmt_match
                        | stmt_subclass
                        | empty
                        | comment

let stmt_block_noarg_re = "pool"
                        | "group"
                        | "allow-update"

let stmt_block_noarg (body:lens)
                        = [ indent 
                        . key stmt_block_noarg_re
                        . sep_obr 
                        . body*
                        . sep_cbr ]

let stmt_block_arg_re = "host"
                      | "class"
                      | "shared-network"
                      | /failover[ ]+peer/
                      | "zone"
                      | "key"

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
