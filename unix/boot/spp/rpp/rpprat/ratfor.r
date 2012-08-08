#-h-  ratfor			 4496  local   12/01/80  15:53:43
# Ratfor preprocessor
   include  defs

   subroutine ratfor

#   DRIVER(ratfor)   Not used; RPP has a C main.

   include COMMON_BLOCKS

   integer i, n
   integer getarg, open

   character arg (FILENAMESIZE)

   STDEFNS 	# define standard definitions file

   call initkw	# initialize variables

   # Read file containing standard definitions
   # If this isn't desired, define (STDEFNS,"")

   if (defns (1) != EOS) {
      infile (1) = open (defns, READ)
      if (infile (1) == ERR)
	 call remark ("can't open standard definitions file.")
      else {
	 call finit
	 call parse
	 call close (infile (1))
      }
   }

   n = 1
   for (i=1;  getarg(i,arg,FILENAMESIZE) != EOF;  i=i+1) {
      n = n + 1
      call query ("usage:  ratfor [-g] [files] >outfile.")
      if (arg(1) == MINUS & arg(2) == LETG & arg(3) == EOS) {
	 dbgout = YES
	 next
      } else if (arg(1) == MINUS & arg(2) == EOS) {
	 infile(1) = STDIN
         call finit
      } else {
	 infile(1) = open (arg, READ)
	 if (infile(1) == ERR) {
	    call cant (arg)
	 } else {			#save file name for error messages
            call finit
	    call scopy (arg, 1, fnames, 1)
	    for (fnamp=1;  fnames(fnamp) != EOS;  fnamp=fnamp+1)
		if (fnames(fnamp) == PERIOD & fnames(fnamp+1) == LETR)
		    fnames(fnamp+1) = LETX
	 }
      }
      call parse
      if (infile (1) != STDIN)
	 call close (infile (1))
   }

   if (n == 1) {   # no files given on command line, use STDIN
      infile (1) = STDIN
      call finit
      call parse
   }

   call lndict

#   DRETURN
   end
