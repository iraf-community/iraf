#-h-  domac			  326  local   12/01/80  15:53:49
# domac - install macro definition in table
   include  defs

   subroutine domac (argstk, i, j)
   integer argstk (ARGSIZE), i, j

   include COMMON_BLOCKS

   integer a2, a3

   if (j - i > 2) {
      a2 = argstk (i + 2)
      a3 = argstk (i + 3)
      call entdef (evalst (a2), evalst (a3), deftbl)	 # subarrays
      }
   return
   end
