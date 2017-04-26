#-h-  finit			  432  local   12/01/80  15:54:07
# finit - initialize for each input file
   include  defs

   subroutine finit

   include COMMON_BLOCKS

   outp = 0  # output character pointer
   level = 1  # file control
   linect (1) = 0
   sbp	= 1
   fnamp = 2
   fnames (1) = EOS
   bp = PBPOINT
   buf (bp) = EOS    # to force a read on next call to 'ngetch'
   fordep = 0  # for stack
   fcname (1) = EOS # current function name
   swtop = 0  # switch stack
   swlast = 1
   swvnum = 0
   swvlev = 0
   return
   end
