#-h-  whiles			  148  local   12/01/80  15:55:12
# whiles - generate code for end of while
   include  defs

   subroutine whiles (lab)

   integer lab
   include COMMON_BLOCKS

   call outgo (lab)
   call indent (-1)
   call outcon (lab + 1)
   return
   end
