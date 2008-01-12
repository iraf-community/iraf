#-h-  alldig			  306  local   12/01/80  15:53:45
# alldig - return YES if str is all digits
   include  defs

   integer function alldig (str)
   character str (ARB)
   integer i

   alldig = NO
   if (str (1) == EOS)
      return
   for (i = 1; str (i) != EOS; i = i + 1)
      if (!IS_DIGIT(str (i)))
	 return
   alldig = YES
   return
   end
