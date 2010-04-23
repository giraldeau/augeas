module Httpd = 
    autoload xfm

(* Helpers *)

(* Newlines can be escaped. *)
let sep = del /([ \t]+(\\\\\n)?)+/ " "
let eol = del /([ \t]*(\\\\\n)?)*\n/ "\n"

let ws = /[ \t]*/
let alnum = /[a-zA-Z0-9_]+/ - /Proxy|Directory|DirectoryMatch|Files|FilesMatch|Location|LocationMatch|AuthnProviderAlias|IfDefine|IfVersion|Limit|LimitExcept|Proxy|ProxyMatch|VirtualHost|IfModule/
(* the last character in the non-quoted word must not be a backslash. I guess this is not completely semantically
   correct but at the same time, apache discourages to use backslashes for anything else than 
   line breaking so we should be safe here. This restriction is in place to support 
   escaped new lines in the sep and eol rules. *)
let word = /\"([^\"\n]|\\\\\")*\"|'([^'\n]|\\\\')*'|[^'" \t\n]*[^'" \t\n\\]/
let secarg = /\"([^\"\n]|\\\\\")*\"|'([^'\n]|\\\\')*'|[^'" \t\n>]+/
let wskey (k:regexp) = del ws "" . key k
let params (param:regexp) = [ sep . label "param" . store param ]*
let sec (name:string) (body:lens) = 
    [ del ws "" . Util.del_str "<" . key name . params secarg . Util.del_str ">" . eol . 
        body . del ws "" . Util.del_str("</" . name . ">") . eol ]
    
(* Definitions *)
let comment = [ del /([ \t]*(#.*)*)\n/ "#\n" ]
let directive = [ wskey alnum . params word . eol ]

let section (name:string) = sec name (directive|comment)*
    
let sections = section "Directory"
             | section "DirectoryMatch"
             | section "Files"
             | section "FilesMatch"
             | section "Location"
             | section "LocationMatch"
             | section "AuthnProviderAlias"
             | section "IfDefine"
             | section "IfVersion"
             | section "Limit"
             | section "LimitExcept"
             | section "Proxy"
             | section "ProxyMatch"


let virtualHost = sec "VirtualHost" (directive|comment|sections)*


(*let rec nesIf  = sec "IfModule" (nesIf|directive|comment)* *)
let rec nesIf  =  [ del ws "" . Util.del_str "<" . key "IfModule" . params secarg . Util.del_str ">" . eol . 
        (directive|comment|nesIf)* . del ws "" . Util.del_str("</IfModule>") . eol ]
(* What we want ot say is *)
(* let rec body = (directive|comment)* | ifModule body | directory body | ... *)
(* but we can't typecheck that *)

(* FIXME: *)
(* - Nesting of sections *)
let lns = (directive | comment | virtualHost| sections| nesIf)*

let filter =
    incl "/etc/httpd/conf/httpd.conf" .
    Util.stdexcl

let xfm = transform lns filter
