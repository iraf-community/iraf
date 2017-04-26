include	defs

# slstr --- slice a substring from a string

   integer function slstr (from, to, first, chars)
   character from (ARB), to (ARB)
   integer first, chars

   integer len, i, j, k
   integer length

   len = length (from)

   i = first
   if (i < 1)
      i = i + len + 1

   if (chars < 0) {
      i = i + chars + 1
      chars = - chars
      }

   j = i + chars - 1
   if (i < 1)
      i = 1
   if (j > len)
      j = len

   for (k = 0; i <= j; k = k + 1) {
      to (k + 1) = from (i)
      i = i + 1
      }
   to (k + 1) = EOS

   return (k)
   end
