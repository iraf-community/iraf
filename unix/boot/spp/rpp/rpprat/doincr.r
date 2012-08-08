#-h-  doincr			  246  local   12/01/80  15:53:49
# doincr - increment macro argument by 1
   include  defs

   subroutine doincr (argstk, i, j)
   integer argstk (ARGSIZE), i, j

   include COMMON_BLOCKS

   integer k
   integer ctoi

   k = argstk (i + 2)
   call pbnum (ctoi (evalst, k) + 1)

   return
   end
