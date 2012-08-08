include	defs

# fcopy - copy file in to file out

   subroutine fcopy (in, out)
   filedes in, out

   character line (MAXLINE)

   integer getlin

   while (getlin (line, in) != EOF)
      call putlin (line, out)

   return
   end
