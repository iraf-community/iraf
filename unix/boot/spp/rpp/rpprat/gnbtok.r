#-h-  gnbtok			  237  local   12/01/80  15:54:09
# gnbtok - get nonblank token
   include  defs

   character function gnbtok (token, toksiz)
   character token (MAXTOK)
   integer toksiz

   include COMMON_BLOCKS

   character gettok

   call skpblk
   repeat {
      gnbtok = gettok (token, toksiz)
   } until (gnbtok != BLANK)

   return
   end
