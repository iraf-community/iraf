#-h-  docode			  522  local   12/01/80  15:53:49
# docode - generate code for beginning of do
   include  defs

   subroutine docode (lab)
   integer lab

   integer labgen

   include COMMON_BLOCKS

   character gnbtok
   character lexstr (MAXTOK)

   string sdo "do"

   xfer = NO
   call outtab
   call outstr (sdo)
   call outch (BLANK)
   lab = labgen (2)
   if (gnbtok (lexstr, MAXTOK) == DIGIT) # check for fortran DO
      call outstr (lexstr)
   else {
      call pbstr (lexstr)
      call outnum (lab)
      }
   call outch (BLANK)
   call eatup
   call outdwe
   call indent (1)
   return
   end
