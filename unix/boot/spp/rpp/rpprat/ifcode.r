#-h-  ifcode			  198  local   12/01/80  15:54:10
# ifcode - generate initial code for if
   include  defs

   subroutine ifcode (lab)
   integer lab

   include COMMON_BLOCKS

   integer labgen

   xfer = NO
   lab = labgen (2)
   call ifgo (lab)
   call indent (1)
   return
   end
