#-h-  initkw			  549  local   12/01/80  15:54:11
# initkw - initialize tables and important global variables
   include  defs

   subroutine initkw

   include COMMON_BLOCKS

   pointer mktabl

   call dsinit (MEMSIZE)
   deftbl = mktabl (1)	   # symbol table for definitions
   call entdkw
   rkwtbl = mktabl (1)	   # symbol table for Ratfor key words
   call entrkw
   fkwtbl = mktabl (0)	   # symbol table for Fortran key words
   call entfkw
   namtbl = mktabl (1)	   # symbol table for long identifiers
   xpptbl = mktabl (1)	   # symbol table for xpp directives
   call entxkw
   gentbl = mktabl (0)	   # symbol table for generated identifiers
   errtbl = NULL	   # table of names to be error checked

   label = FIRST_LABEL	   # starting statement label
   smem(1) = EOS	   # haven't read in "mem.com" file yet
   body = NO		   # not in procedure body to start
   dbgout = NO		   # disable debug output by default
   dbglev = 0		   # file level if debug enabled
   memflg = NO		   # haven't declared mem common yet
   swinrg = NO		   # default range checking for switches
   col = 6

   return
   end
