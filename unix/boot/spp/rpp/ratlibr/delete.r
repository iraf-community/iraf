include	defs

# delete --- remove a symbol from the symbol table

   subroutine delete (symbol, st)
   character symbol (ARB)
   pointer st

   DS_DECL(Mem, MEMSIZE)

   integer stlu

   pointer node, pred

   if (stlu (symbol, node, pred, st) == YES) {
      Mem (pred + ST_LINK) = Mem (node + ST_LINK)
      call dsfree (node)
      }

   return
   end
