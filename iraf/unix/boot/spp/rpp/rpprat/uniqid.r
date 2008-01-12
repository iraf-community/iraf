#-h-  uniqid			 1825  local   12/01/80  15:55:09
# uniqid - convert an identifier to one never before seen
   include  defs

subroutine uniqid (id)

character id (MAXTOK)
integer i, j, junk, idchl
external index
integer lookup, index, length
character start (MAXIDLENGTH)
include COMMON_BLOCKS
string idch "0123456789abcdefghijklmnopqrstuvwxyz" # legal id characters

   # Pad the identifer out to length 6 with FILLCHARs:
   for (i = 1; id (i) != EOS; i = i + 1)
      ;
   for (; i <= MAXIDLENGTH; i = i + 1)
      id (i) = FILLCHAR
   i = MAXIDLENGTH + 1
   id (i) = EOS
   id (i - 1) = FILLCHAR

   # Look it up in the table of generated names.  If it's not there,
   #  it's unique.  If it is there, it has been generated previously;
   #  modify it and try again.	Assume this procedure always succeeds,
   #  since to fail implies there are very, very many identifiers in
   #  the symbol table.
   #  Note that we must preserve the first and last characters of the
   #  id, so as not to disturb implicit typing and to provide a flag
   #  to catch potentially conflicting user-defined identifiers without
   #  a lookup.

   if (lookup (id, junk, gentbl) == YES) {   # (not very likely)
      idchl = length (idch)
      for (i = 2; i < MAXIDLENGTH; i = i + 1)
	 start (i) = id (i)
      repeat {	  # until we get a unique id
	 for (i = MAXIDLENGTH - 1; i > 1; i = i - 1) {
	    j = mod (index (idch, id (i)), idchl) + 1
	    id (i) = idch (j)
	    if (id (i) != start (i))
	       break
	    }
	 if (i == 1)
	    call baderr ("cannot make identifier unique.")
	 } until (lookup (id, junk, gentbl) == NO)
      }
end
