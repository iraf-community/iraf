#-h-  labgen			  189  local   12/01/80  15:54:12
# labgen - generate  n	consecutive labels, return first one
   include  defs

   integer function labgen (n)
   integer n

   include COMMON_BLOCKS

   labgen = label
   label = label + (n / 10 + 1) * 10
   return
   end
