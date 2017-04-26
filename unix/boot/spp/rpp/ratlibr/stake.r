include	defs

# stake --- take characters from a string APL-style

   integer function stake (from, to, chars)
   character from (ARB), to (ARB)
   integer chars

   integer len, start
   integer length, ctoc, max0

   len = length (from)
   if (chars < 0) {
      start = max0 (len + chars, 0)
      return (ctoc (from (start + 1), to, len + 1))
      }
   else
      return (ctoc (from, to, chars + 1))

   end
