include	defs

# scopy - copy string at from (i) to to (j)

   subroutine scopy (from, i, to, j)
   character from (ARB), to (ARB)
   integer i, j

   integer k1, k2

   k2 = j
   for (k1 = i; from (k1) != EOS; k1 = k1 + 1) {
      to (k2) = from (k1)
      k2 = k2 + 1
      }
   to (k2) = EOS

   return
   end
