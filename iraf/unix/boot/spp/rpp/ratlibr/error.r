include	defs

# error - print message and terminate execution

   subroutine error (line)
   character line (ARB)

   call remark (line)
   call endst
   end
