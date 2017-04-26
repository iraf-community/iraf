#-h-  swcode			  746  local   12/01/80  15:55:06
# swcode - generate code for beginning of switch statement
   include  defs

   subroutine swcode (lab)
   integer lab

   include COMMON_BLOCKS

   character tok (MAXTOK)

   integer labgen, gnbtok

   lab = labgen (2)
   swvnum = swvnum + 1
   swvlev = swvlev + 1
   if (swvlev > MAXSWNEST)
      call baderr ("switches nested too deeply.")
   swvstk(swvlev) = swvnum

   if (swlast + 3 > MAXSWITCH)
      call baderr ("switch table overflow.")
   swstak (swlast) = swtop
   swstak (swlast + 1) = 0
   swstak (swlast + 2) = 0
   swtop = swlast
   swlast = swlast + 3
   xfer = NO
   call outtab	# Innn=(e)
   call swvar (swvnum)
   call outch (EQUALS)
   call balpar
   call outdwe
   call outgo (lab) # goto L
   call indent (1)
   xfer = YES
   while (gnbtok (tok, MAXTOK) == NEWLINE)
      ;
   if (tok (1) != LBRACE) {
      call synerr ("missing left brace in switch statement.")
      call pbstr (tok)
      }
   return
   end
