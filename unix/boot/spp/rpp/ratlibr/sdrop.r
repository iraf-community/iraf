include	defs

# sdrop --- drop characters from a string APL-style

   integer function sdrop (from, to, chars)
   character from (ARB), to (ARB)
   integer chars

   integer len, start
   integer ctoc, length, min0

   len = length (from)
   if (chars < 0)
      return (ctoc (from, to, len + chars + 1))
   else {
      start = min0 (chars, len)
      return (ctoc (from (start + 1), to, len + 1))
      }

   end
