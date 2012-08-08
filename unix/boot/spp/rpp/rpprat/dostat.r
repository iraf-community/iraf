#-h-  dostat			  156  local   12/01/80  15:53:50
# dostat - generate code for end of do statement
   include  defs

   subroutine dostat (lab)

   integer lab

   call indent (-1)
   call outcon (lab)
   call outcon (lab + 1)
   return
   end
