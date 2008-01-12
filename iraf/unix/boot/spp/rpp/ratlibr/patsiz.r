include	defs

# patsiz --- returns size of pattern entry at pat (n)

   integer function patsiz (pat, n)
   character pat (MAXPAT)
   integer n

   if (pat (n) == CHAR | pat (n) == START_TAG | pat (n) == STOP_TAG)
      patsiz = 2
   else if (pat (n) == BOL | pat (n) == EOL | pat (n) == ANY)
      patsiz = 1
   else if (pat (n) == CCL | pat (n) == NCCL)
      patsiz = pat (n + 1) + 2
   else if (pat (n) == CLOSURE)      # optional
      patsiz = CLOSIZE
   else
      call error ("in patsiz: can't happen.")

   return
   end
