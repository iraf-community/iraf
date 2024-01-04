include	defs

# lookup --- find a symbol in the symbol table, return its data

   integer function lookup (symbol, info, st)
   character symbol (ARB)
   integer info (ARB)
   pointer st

   DS_DECL(Mem, MEMSIZE)

   integer i, nodsiz, kluge
   integer stlu

   pointer node, pred

   if (stlu (symbol, node, pred, st) == NO) {
      lookup = NO
      return
      }

   nodsiz = Mem (st)
   for (i = 1; i <= nodsiz; i = i + 1) {
      kluge = node + ST_DATA - 1 + i
      info (i) = Mem (kluge)
      }
   lookup = YES

   return
   end
