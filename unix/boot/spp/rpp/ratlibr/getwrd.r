include	defs

# getwrd - get non-blank word from in (i) into out, increment i

   integer function getwrd (in, i, out)
   character in (ARB), out (ARB)
   integer i

   integer j

   while (in (i) == BLANK | in (i) == TAB)
      i = i + 1

   j = 1
   while (in (i) != EOS & in (i) != BLANK
    & in (i) != TAB & in (i) != NEWLINE) {
      out (j) = in (i)
      i = i + 1
      j = j + 1
      }
   out (j) = EOS

   getwrd = j - 1
   return
   end
