#-h-  whilec			  262  local   12/01/80  15:55:11
# whilec - generate code for beginning of while
   include  defs

   subroutine whilec (lab)

   integer lab
   integer labgen
   include COMMON_BLOCKS

   call outcon (0)    # unlabeled continue, in case there was a label
   lab = labgen (2)
   call outnum (lab)
   call ifgo (lab + 1)
   call indent (1)
   return
   end
