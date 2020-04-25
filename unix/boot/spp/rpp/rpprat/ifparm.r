#-h-  ifparm			  689  local   12/01/80  15:54:11
# ifparm - determines if the defined symbol has arguments in its
   include  defs
# definition.  This effects how the macro is expanded.

   integer function ifparm (strng)
   character strng (ARB)

   character c

   external index
   integer i, index, type

   c = strng (1)
   if (c == INCTYPE | c == SUBTYPE | c == IFTYPE | c == ARITHTYPE |
    c == MACTYPE)
      ifparm = YES
   else {
      ifparm = NO
      for (i = 1; index (strng (i), ARGFLAG) > 0; ) {
	 i = i + index (strng (i), ARGFLAG) # i points at char after ARGFLAG
	 if ((type (strng (i)) == DIGIT)
	   & (type (strng (i + 1)) != DIGIT)) {
	    ifparm = YES
	    break
	 }
      }
   }
   return
   end
