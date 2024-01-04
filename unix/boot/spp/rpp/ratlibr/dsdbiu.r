include	defs

# dsdbiu --- dump contents of block-in-use

   subroutine dsdbiu (b, form)
   pointer b
   character form

   DS_DECL(Mem, MEMSIZE)

   integer l, s, lmax

   string blanks "	    "

   call putint (b, 5, ERROUT)
   call putch (BLANK, ERROUT)
   call putint (Mem (b + DS_SIZE), 0, ERROUT)
   call remark (" words in use.")

   l = 0
   s = b + Mem (b + DS_SIZE)
   if (form == DIGIT)
      lmax = 5
   else
      lmax = 50

   for (b = b + DS_OHEAD; b < s; b = b + 1) {
      if (l == 0)
	 call putlin (blanks, ERROUT)
      if (form == DIGIT)
	 call putint (Mem (b), 10, ERROUT)
      elif (form == LETTER)
	 call putch (Mem (b), ERROUT)
      l = l + 1
      if (l >= lmax) {
	 l = 0
	 call putch (NEWLINE, ERROUT)
	 }
      }

   if (l != 0)
      call putch (NEWLINE, ERROUT)

   return
   end
