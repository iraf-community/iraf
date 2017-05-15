include	defs

# stlu --- symbol table lookup primitive

   integer function stlu (symbol, node, pred, st)
   character symbol (ARB)
   pointer node, pred, st

   DS_DECL(Mem, MEMSIZE)

   integer hash, i, j, nodsiz

   nodsiz = Mem (st)

   hash = 0
   for (i = 1; symbol (i) != EOS; i = i + 1)
      hash = hash + symbol (i)
   hash = mod (hash, ST_HTABSIZE) + 1

   pred = st + hash
   node = Mem (pred)
   while (node != LAMBDA) {
      i = 1
      j = node + ST_DATA + nodsiz
      while (symbol (i) == Mem (j)) {
	 if (symbol (i) == EOS)
	    return (YES)
	 i = i + 1
	 j = j + 1
	 }
      pred = node
      node = Mem (pred + ST_LINK)
      }

   return (NO)
   end
