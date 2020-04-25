include  defs

# SQUASH - convert a long or special identifier into a Fortran variable

subroutine squash (id)

character id(MAXTOK)
integer junk, i, j
integer lookup, ludef
character newid(MAXTOK), recdid(MAXTOK)
include COMMON_BLOCKS

	# identify names for which error checking is to be performed
	if (body == YES & errtbl != NULL & ername == NO)
	    if (lookup (id, junk, errtbl) == YES)
		ername = YES

	j = 1
	for (i=1;  id(i) != EOS;  i=i+1)		# copy, delete '_'
	    if ((BIGA <= id(i) & id(i) <= BIGZ)
              | (LETA <= id(i) & id(i) <= LETZ)
              | (DIG0 <= id(i) & id(i) <= DIG9)) {
		newid(j) = id(i)
	        j = j + 1
	    }
	newid(j) = EOS

	# done if ordinary (short) Fortran variable
	if (i-1 < MAXIDLENGTH & i == j)
	    return

# Otherwise, the identifier (1) is longer than Fortran allows,
# (2) contains special characters (_ or .), or (3) is the maximum
# length permitted by the Fortran compiler.  The first two cases
# obviously call for name conversion; the last case may require conversion
# to avoid accidental conflicts with automatically generated names.

	if (lookup (id, junk, fkwtbl) == YES)	# Fortran key word?
	    return				# (must be treated as reserved)

	if (ludef (id, recdid, namtbl) == YES) {   # have we seen this before?
	    call scopy (recdid, 1, id, 1)
	    return
	}
		
	call mapid (newid)			# try standard mapping
	if (lookup (newid, junk, gentbl) == YES) {
	    call synerr ("Warning: identifier mapping not unique.")
	    call uniqid (newid)
	}
	call entdef (newid, id, gentbl)

	call entdef (id, newid, namtbl)		# record it for posterity
	call scopy (newid, 1, id, 1)		# substitute it for the old one
end
