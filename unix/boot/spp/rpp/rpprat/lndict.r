#-h-  lndict			  678  local   12/01/80  15:54:13
# lndict - output long-name dictionary as a debugging aid
   include  defs

subroutine lndict

character sym (MAXTOK), c
integer sctabl, length
pointer posn, locn
include COMMON_BLOCKS

   posn = 0
   while (sctabl (namtbl, sym, locn, posn) != EOF)
      if (length(sym) > MAXIDLENGTH) {
	  call outch (LETC)
	  call outtab
	  for (; mem (locn) != EOS; locn = locn + 1) {
	     c = mem (locn) # kluge for people with LOGICAL*1 characters
	     call outch (c)
	  }
	  call outch (BLANK)
	  call outch (BLANK)
	  call outstr (sym)
	  call outdon
      }
   return
end
