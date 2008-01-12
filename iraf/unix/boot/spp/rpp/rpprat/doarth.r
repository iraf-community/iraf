#-h-  doarth			  636  local   12/01/80  15:53:48
# doarth - do arithmetic operation
   include  defs

   subroutine doarth (argstk, i, j)
   integer argstk (ARGSIZE), i, j

   include COMMON_BLOCKS

   integer k, l
   integer ctoi

   character op

   k = argstk (i + 2)
   l = argstk (i + 4)
   op = evalst (argstk (i + 3))
   if (op == PLUS)
      call pbnum (ctoi (evalst, k) + ctoi (evalst, l))
   else if (op == MINUS)
      call pbnum (ctoi (evalst, k) - ctoi (evalst, l))
   else if (op ==  STAR )
      call pbnum (ctoi (evalst, k) * ctoi (evalst, l))
   else if (op ==  SLASH )
      call pbnum (ctoi (evalst, k) / ctoi (evalst, l))
   else
      call remark ('arith error')

   return
   end
