include	defs

# mktabl --- make a new (empty) symbol table

   pointer function mktabl (nodsiz)
   integer nodsiz

   DS_DECL(Mem, MEMSIZE)

   pointer st
   pointer dsget

   integer i

   st = dsget (ST_HTABSIZE + 1)     # +1 for record of nodsiz
   Mem (st) = nodsiz
   mktabl = st
   do i = 1, ST_HTABSIZE; {
      st = st + 1
      Mem (st) = LAMBDA 	    # null link
      }

   return
   end
