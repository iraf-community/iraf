include	defs

# query - print usage message if user has requested one

   subroutine query (mesg)
   character mesg (ARB)

   integer getarg

   character arg1 (3), arg2 (1)

   if (getarg (1, arg1, 3) != EOF & getarg (2, arg2, 1) == EOF)
      if (arg1 (1) == QMARK & arg1 (2) == EOS)
	 call error (mesg)

   return
   end
