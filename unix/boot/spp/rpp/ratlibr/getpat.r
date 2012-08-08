include	defs

# getpat - convert str into pattern

   integer function getpat (str, pat)
   character str (ARB), pat (ARB)

   integer makpat

   return (makpat (str, 1, EOS, pat))

   end
