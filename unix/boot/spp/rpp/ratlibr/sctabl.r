include	defs

# sctabl --- scan symbol table, returning next entry or EOF

   integer function sctabl (table, sym, info, posn)
   pointer table, posn
   character sym (ARB)
   integer info (ARB)

   DS_DECL(Mem, MEMSIZE)

   pointer bucket, walker
   pointer dsget

   integer nodsiz, i, j

   if (posn == 0) {		    # just starting scan?
      posn = dsget (2)		       # get space for position info
      Mem (posn) = 1		       # get index of first bucket
      Mem (posn + 1) = Mem (table + 1) # get pointer to first chain
      }

   bucket = Mem (posn)		    # recover previous position
   walker = Mem (posn + 1)
   nodsiz = Mem (table)

   repeat {    # until the next symbol, or none are left
      if (walker != LAMBDA) {	    # symbol available?
	 i = walker + ST_DATA + nodsiz
	 j = 1
	 while (Mem (i) != EOS) {
	    sym (j) = Mem (i)
	    i = i + 1
	    j = j + 1
	    }
	 sym (j) = EOS
	 for (i = 1; i <= nodsiz; i = i + 1) {
	    j = walker + ST_DATA + i - 1
	    info (i) = Mem (j)
	    }
	 Mem (posn) = bucket	    # save position of next symbol
	 Mem (posn + 1) = Mem (walker + ST_LINK)
	 sctabl = 1  # not EOF
	 return
	 }
      else {
	 bucket = bucket + 1
	 if (bucket > ST_HTABSIZE)
	    break
	 j = table + bucket
	 walker = Mem (j)
	 }
      }

   call dsfree (posn)	   # throw away position information
   posn = 0
   sctabl = EOF
   return
   end
