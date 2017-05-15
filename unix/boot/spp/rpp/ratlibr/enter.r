include	defs

# enter --- place a symbol in the symbol table, updating if already present

   subroutine enter (symbol, info, st)
   character symbol (ARB)
   integer info (ARB)
   pointer st

   DS_DECL(Mem, MEMSIZE)

   integer i, nodsiz, j
   integer stlu, length

   pointer node, pred
   pointer dsget

   nodsiz = Mem (st)

   if (stlu (symbol, node, pred, st) == NO) {
      node = dsget (1 + nodsiz + length (symbol) + 1)
      Mem (node + ST_LINK) = LAMBDA
      Mem (pred + ST_LINK) = node
      i = 1
      j = node + ST_DATA + nodsiz
      while (symbol (i) != EOS) {
	 Mem (j) = symbol (i)
	 i = i + 1
	 j = j + 1
	 }
      Mem (j) = EOS
      }

   for (i = 1; i <= nodsiz; i = i + 1) {
      j = node + ST_DATA + i - 1
      Mem (j) = info (i)
      }

   return
   end
