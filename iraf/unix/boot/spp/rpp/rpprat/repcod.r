#-h-  repcod			  262  local   12/01/80  15:54:35
# repcod - generate code for beginning of repeat
   include  defs

   subroutine repcod (lab)
   integer lab

   integer labgen

   call outcon (0)   # in case there was a label
   lab = labgen (3)
   call outcon (lab)
   lab = lab + 1   # label to go on next's
   call indent (1)
   return
   end
