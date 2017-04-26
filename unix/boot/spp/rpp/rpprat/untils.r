#-h-  untils			  397  local   12/01/80  15:55:11
# untils - generate code for until or end of repeat
   include  defs

   subroutine untils (lab, token)
   integer lab, token

   include COMMON_BLOCKS

   character ptoken (MAXTOK)

   integer junk
   integer lex

   xfer = NO
   call outnum (lab)
   if (token == LEXUNTIL) {
      junk = lex (ptoken)
      call ifgo (lab - 1)
      }
   else
      call outgo (lab - 1)
   call indent (-1)
   call outcon (lab + 1)
   return
   end
