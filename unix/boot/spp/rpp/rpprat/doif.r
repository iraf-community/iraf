#-h-  doif			  458  local   12/01/80  15:53:49
# doif - select one of two (macro) arguments
   include  defs

   subroutine doif (argstk, i, j)
   integer argstk (ARGSIZE), i, j

   include COMMON_BLOCKS

   integer a2, a3, a4, a5
   integer equal

   if (j - i < 5)
      return
   a2 = argstk (i + 2)
   a3 = argstk (i + 3)
   a4 = argstk (i + 4)
   a5 = argstk (i + 5)
   if (equal (evalst (a2), evalst (a3)) == YES)   # subarrays
      call pbstr (evalst (a4))
   else
      call pbstr (evalst (a5))

   return
   end
