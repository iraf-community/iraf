#-h-  elseif			  155  local   12/01/80  15:53:51
# elseif - generate code for end of if before else
   include  defs

   subroutine elseif (lab)
   integer lab

   call outgo (lab+1)
   call indent (-1)
   call outcon (lab)
   call indent (1)
   return
   end
