#-h-  fors			  458  local   12/01/80  15:54:08
# fors - process end of for statement
   include  defs

   subroutine fors (lab)
   integer lab

   include COMMON_BLOCKS

   integer i, j
   integer length

   xfer = NO
   call outnum (lab)
   j = 1
   for (i = 1; i < fordep; i = i + 1)
      j = j + length (forstk (j)) + 1
   if (length (forstk (j)) > 0) {
      call outtab
      call outstr (forstk (j))
      call outdon
      }
   call outgo (lab - 1)
   call indent (-1)
   call outcon (lab + 1)
   fordep = fordep - 1
   ername = NO
   return
   end
