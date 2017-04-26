include	defs

# skipbl - skip blanks and tabs at lin(i)

   subroutine skipbl(lin, i)
   character lin(ARB)
   integer i

   while (lin (i) == BLANK | lin (i) == TAB)
      i = i + 1

   return
   end
