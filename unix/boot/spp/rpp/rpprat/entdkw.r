#-h-  entdkw			  975  local   12/01/80  15:54:05
# entdkw --- install macro processor keywords
   include  defs

   subroutine entdkw

   character deft(2), prag(2) #, inct(2), subt(2), ift(2), art(2),
      # ifdft(2), ifndt(2), mact(2)

   string defnam "define"
   string prgnam "pragma"
#  string macnam "mdefine"
#  string incnam "incr"
#  string subnam "substr"
#  string ifnam "ifelse"
#  string arnam "arith"
#  string ifdfnm "ifdef"
#  string ifndnm "ifnotdef"

   data deft (1), deft (2) /DEFTYPE, EOS/
   data prag (1), prag (2) /PRAGMATYPE, EOS/
#  data mact (1), mact (2) /MACTYPE, EOS/
#  data inct (1), inct (2) /INCTYPE, EOS/
#  data subt (1), subt (2) /SUBTYPE, EOS/
#  data ift (1), ift (2) /IFTYPE, EOS/
#  data art (1), art (2) /ARITHTYPE, EOS/
#  data ifdft (1), ifdft (2) /IFDEFTYPE, EOS/
#  data ifndt (1), ifndt (2) /IFNOTDEFTYPE, EOS/

   call ulstal (defnam, deft)
   call ulstal (prgnam, prag)
#  call ulstal (macnam, mact)
#  call ulstal (incnam, inct)
#  call ulstal (subnam, subt)
#  call ulstal (ifnam, ift)
#  call ulstal (arnam, art)
#  call ulstal (ifdfnm, ifdft)
#  call ulstal (ifndnm, ifndt)

return
end
