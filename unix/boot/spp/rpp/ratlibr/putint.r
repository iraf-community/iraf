include	defs

# putint - output integer in specified field

   subroutine putint (n, w, fd)
   integer n, w
   filedes fd

   character chars (MAXCHARS)

   integer junk
   integer itoc

   junk = itoc (n, chars, MAXCHARS)
   call putstr (chars, w, fd)

   return
   end
