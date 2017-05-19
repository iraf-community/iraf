include	defs

# dsget --- get pointer to block of at least w available words

   pointer function dsget (w)
   integer w

   DS_DECL(Mem, MEMSIZE)

   pointer p, q, l

   integer n, k, junk
   integer getlin

   character c (10)

   n = w + DS_OHEAD
   q = DS_AVAIL

   repeat {
      p = Mem (q + DS_LINK)
      if (p == LAMBDA) {
	 call remark ("in dsget: out of storage space.")
	 call remark ("type 'c' or 'i' for char or integer dump.")
	 junk = getlin (c, STDIN)
	 if (c (1) == LETC | c (1) == BIGC)
	    call dsdump (LETTER)
	 else if (c (1) == LETI | c (1) == BIGI)
	    call dsdump (DIGIT)
	 call error ("program terminated.")
	 }
      if (Mem (p + DS_SIZE) >= n)
	 break
      q = p
      }

   k = Mem (p + DS_SIZE) - n
   if (k >= DS_CLOSE) {
      Mem (p + DS_SIZE) = k
      l = p + k
      Mem (l + DS_SIZE) = n
      }
   else {
      Mem (q + DS_LINK) = Mem (p + DS_LINK)
      l = p
      }

   return (l + DS_OHEAD)

   end
