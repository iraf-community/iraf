include	defs

# stclos --- insert closure entry at pat (j)

   integer function stclos (pat, j, lastj, lastcl)
   character pat (MAXPAT)
   integer j, lastj, lastcl

   integer addset
   integer jp, jt, junk

   for (jp = j - 1; jp >= lastj; jp = jp - 1) {   # make a hole
      jt = jp + CLOSIZE
      junk = addset (pat (jp), pat, jt, MAXPAT)
      }
   j = j + CLOSIZE
   stclos = lastj
   junk = addset (CLOSURE, pat, lastj, MAXPAT)	 # put closure in it
   junk = addset (0, pat, lastj, MAXPAT)	 # COUNT
   junk = addset (lastcl, pat, lastj, MAXPAT)	 # PREVCL
   junk = addset (0, pat, lastj, MAXPAT)	 # START

   return
   end
