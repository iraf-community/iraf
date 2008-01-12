include	defs

# omatch --- try to match a single pattern at pat (j)

   integer function omatch (lin, i, pat, j)
   character lin (MAXLINE), pat (MAXPAT)
   integer i, j

   integer bump
   integer locate

   omatch = NO
   if (lin (i) == EOS)
      return
   bump = -1
   if (pat (j) == CHAR) {
      if (lin (i) == pat (j + 1))
	 bump = 1
      }
   else if (pat (j) == BOL) {
      if (i == 1)
	 bump = 0
      }
   else if (pat (j) == ANY) {
      if (lin (i) != NEWLINE)
	 bump = 1
      }
   else if (pat (j) == EOL) {
      if (lin (i) == NEWLINE)
	 bump = 0
      }
   else if (pat (j) == CCL) {
      if (locate (lin (i), pat, j + 1) == YES)
	 bump = 1
      }
   else if (pat (j) == NCCL) {
      if (lin (i) != NEWLINE & locate (lin (i), pat, j + 1) == NO)
	 bump = 1
      }
   else
      call error ("in omatch: can't happen.")
   if (bump >= 0) {
      i = i + bump
      omatch = YES
      }

   return
   end
