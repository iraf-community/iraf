#-h-  ludef			  495  local   12/01/80  15:54:29
# ludef --- look up a defined identifier, return its definition
   include  defs

   integer function ludef (id, defn, table)
   character id (ARB), defn (ARB)
   pointer table

   include COMMON_BLOCKS

   integer i
   integer lookup

   pointer locn

   ludef = lookup (id, locn, table)
   if (ludef == YES) {
      i = 1
      for (; mem (locn) != EOS; locn = locn + 1) {
	 defn (i) = mem (locn)
	 i = i + 1
	 }
      defn (i) = EOS
      }
   else
      defn (1) = EOS

   return
   end
