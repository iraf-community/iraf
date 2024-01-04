include	defs

# dsinit --- initialize dynamic storage space to w words

   subroutine dsinit (w)
   integer w

   DS_DECL(Mem, MEMSIZE)

   pointer t

   if (w < 2 * DS_OHEAD + 2)
      call error ("in dsinit: unreasonably small memory size.")

   # set up avail list:
   t = DS_AVAIL
   Mem (t + DS_SIZE) = 0
   Mem (t + DS_LINK) = DS_AVAIL + DS_OHEAD

   # set up first block of space:
   t = DS_AVAIL + DS_OHEAD
   Mem (t + DS_SIZE) = w - DS_OHEAD - 1     # -1 for MEMEND
   Mem (t + DS_LINK) = LAMBDA

   # record end of memory:
   Mem (DS_MEMEND) = w

   return
   end
