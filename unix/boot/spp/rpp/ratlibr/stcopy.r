include	defs

# stcopy - copy string from in (i) to out (j), updating j, excluding EOS

   subroutine stcopy (in, i, out, j)
   character in (ARB), out (ARB)
   integer i, j

   integer k

   for (k = i; in (k) != EOS; k = k + 1) {
      out (j) = in (k)
      j = j + 1
      }
   out(j) = EOS
   return
   end
