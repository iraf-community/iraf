#-h-  ngetch			  442  local   12/01/80  15:54:30
# ngetch - get a (possibly pushed back) character
   include  defs

   character function ngetch (c)
   character c

   include COMMON_BLOCKS

   integer getlin, n, i

   if (buf (bp) == EOS)
      if (getlin (buf (PBPOINT), infile (level)) == EOF)
	 c = EOF
      else {
	 c = buf (PBPOINT)
	 bp = PBPOINT + 1
	 if (c == SHARP) {		#check for "#!# nn" directive
	    if (buf(bp) == BANG & buf(bp+1) == SHARP) {
	       n = 0
	       for (i=bp+3;  buf(i) >= DIG0 & buf(i) <= DIG9;  i=i+1)
		  n = n * 10 + buf(i) - DIG0
	       linect (level) = n - 1
	       }
	    }
	    if (linect (level) > 0) {
	       linect (level) = linect (level) + 1
	    }
	 }
   else {
      c = buf (bp)
      bp = bp + 1
      }

   return (c)
   end
