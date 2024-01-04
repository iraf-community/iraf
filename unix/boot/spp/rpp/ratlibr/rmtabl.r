include	defs

# rmtabl --- remove a symbol table, deleting all entries

   subroutine rmtabl (st)
   pointer st

   DS_DECL(Mem, MEMSIZE)

   integer i

   pointer walker, bucket, node

   bucket = st
   do i = 1, ST_HTABSIZE; {
      bucket = bucket + 1
      walker = Mem (bucket)
      while (walker != LAMBDA) {
	 node = walker
	 walker = Mem (node + ST_LINK)
	 call dsfree (node)
	 }
      }

   call dsfree (st)
   return
   end
