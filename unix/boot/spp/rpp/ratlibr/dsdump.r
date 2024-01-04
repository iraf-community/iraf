include	defs

# dsdump --- produce semi-readable dump of storage

   subroutine dsdump (form)
   character form

   DS_DECL(Mem, MEMSIZE)

   pointer p, t, q

   t = DS_AVAIL

   call remark ("** DYNAMIC STORAGE DUMP **.")
   call putint (1, 5, ERROUT)
   call putch (BLANK, ERROUT)
   call putint (DS_OHEAD + 1, 0, ERROUT)
   call remark (" words in use.")

   p = Mem (t + DS_LINK)
   while (p != LAMBDA) {
      call putint (p, 5, ERROUT)
      call putch (BLANK, ERROUT)
      call putint (Mem (p + DS_SIZE), 0, ERROUT)
      call remark (" words available.")
      q = p + Mem (p + DS_SIZE)
      while (q != Mem (p + DS_LINK) & q < Mem (DS_MEMEND))
	 call dsdbiu (q, form)
      p = Mem (p + DS_LINK)
      }

   call remark ("** END DUMP **.")
   return
   end
