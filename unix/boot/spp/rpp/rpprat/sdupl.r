#-h-  sdupl			  374  local   12/01/80  15:55:03
# sdupl --- duplicate a string in dynamic storage space
   include  defs

   pointer function sdupl (str)
   character str (ARB)

   DS_COMMON

   integer i
   integer length

   pointer j
   pointer dsget

   j = dsget (length (str) + 1)
   sdupl = j
   for (i = 1; str (i) != EOS; i = i + 1) {
      mem (j) = str (i)
      j = j + 1
      }
   mem (j) = EOS

   return
   end
