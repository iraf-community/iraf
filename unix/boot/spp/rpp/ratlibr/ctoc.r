include	defs

# ctoc --- convert EOS-terminated string to EOS-terminated string

   integer function ctoc (from, to, len)
   integer len
   character from (ARB), to (len)

   integer i

   for (i = 1; i < len & from (i) != EOS; i = i + 1)
      to (i) = from (i)

   to (i) = EOS

   return (i - 1)

   end
