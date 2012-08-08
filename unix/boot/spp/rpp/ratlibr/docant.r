include	defs

# docant
#
# Similar to cant(name), however precede the messge with the name
# of the program that was running when the file could not be
# opened.  Helpful in a pipeline to verify which program was not
# able to open a file.
#
   subroutine  docant(name)

   character  name(ARB), prog(FILENAMESIZE)
   integer    length
   integer    getarg

   length = getarg(0, prog, FILENAMESIZE)
   if (length != EOF) {
      call putlin(prog, STDERR)
      call putch(COLON, STDERR)
      call putch(BLANK, STDERR)
   }
   call cant(name)

   return
   end
