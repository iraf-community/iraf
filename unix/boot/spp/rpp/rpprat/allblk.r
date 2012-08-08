#-h-  allblk			  486  local   12/01/80  15:53:44
# allblk - determine if line consists of all blanks
   include  defs

# this routine is called by outdon, and is here to fix
# a bug which sometimes occurs if two or more includes precede the
# first line of executable code.  Could not trace down the cause

   integer function allblk (buf)
   character buf (ARB)

   integer i

   allblk = YES
   for (i = 1; buf (i) != NEWLINE & buf (i) != EOS; i = i + 1)
      if (buf (i) != BLANK) {
	 allblk = NO
	 break
	 }

   return
   end
