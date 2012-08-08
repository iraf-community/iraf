include	defs

# getccl --- expand char class at arg (i) into pat (j)

   integer function getccl (arg, i, pat, j)
   character arg (MAXARG), pat (MAXPAT)
   integer i, j

   integer jstart, junk
   integer addset

   i = i + 1	  # skip over [
   if (arg (i) == NOT) {
      junk = addset (NCCL, pat, j, MAXPAT)
      i = i + 1
      }
   else
      junk = addset (CCL, pat, j, MAXPAT)
   jstart = j
   junk = addset (0, pat, j, MAXPAT)	  # leave room for count
   call filset (CCLEND, arg, i, pat, j, MAXPAT)
   pat (jstart) = j - jstart - 1
   if (arg (i) == CCLEND)
      getccl = OK
   else
      getccl = ERR

   return
   end
