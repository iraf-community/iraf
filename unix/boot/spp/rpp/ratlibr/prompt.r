include	defs

# prompt - write to/read from teletype

   subroutine prompt (str, buf, fd)
   character str(ARB), buf(ARB)
   filedes fd

   integer isatty

   if (isatty(fd) == YES)
	 {
	 call putlin (str, fd)
	 call flush (fd)
	 }
   call getlin (buf, fd)

   return
   end
