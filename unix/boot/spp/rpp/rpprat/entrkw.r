#-h-  entrkw			 1003  local   12/01/80  15:54:06
# entrkw --- install Ratfor keywords in symbol table
   include  defs

   subroutine entrkw

   include COMMON_BLOCKS

   string sif "if"
   string selse "else"
   string swhile "while"
   string sdo "do"
   string sbreak "break"
   string snext "next"
   string sfor "for"
   string srept "repeat"
   string suntil "until"
   string sret "return"
   string sstr "string"
   string sswtch "switch"
   string scase "case"
   string sdeflt "default"
   string send "end"
   string serrchk "errchk"
   string siferr "iferr"
   string sifnoerr "ifnoerr"
   string sthen "then"
   string sbegin "begin"
   string spoint "pointer"
   string sgoto "goto"

   call enter (sif, LEXIF, rkwtbl)
   call enter (selse, LEXELSE, rkwtbl)
   call enter (swhile, LEXWHILE, rkwtbl)
   call enter (sdo, LEXDO, rkwtbl)
   call enter (sbreak, LEXBREAK, rkwtbl)
   call enter (snext, LEXNEXT, rkwtbl)
   call enter (sfor, LEXFOR, rkwtbl)
   call enter (srept, LEXREPEAT, rkwtbl)
   call enter (suntil, LEXUNTIL, rkwtbl)
   call enter (sret, LEXRETURN, rkwtbl)
   call enter (sstr, LEXSTRING, rkwtbl)
   call enter (sswtch, LEXSWITCH, rkwtbl)
   call enter (scase, LEXCASE, rkwtbl)
   call enter (sdeflt, LEXDEFAULT, rkwtbl)
   call enter (send, LEXEND, rkwtbl)
   call enter (serrchk, LEXERRCHK, rkwtbl)
   call enter (siferr, LEXIFERR, rkwtbl)
   call enter (sifnoerr, LEXIFNOERR, rkwtbl)
   call enter (sthen, LEXTHEN, rkwtbl)
   call enter (sbegin, LEXBEGIN, rkwtbl)
   call enter (spoint, LEXPOINTER, rkwtbl)
   call enter (sgoto, LEXGOTO, rkwtbl)

   return
   end
