include <tbset.h>

#  TM_HC  --  Get column name from image header and copy image into table.
#
#
#
#
#  Revision history:
#  ----------------
#  30-Jan-97  -  Task created (I.Busko)


procedure tm_hc (tp, cp, ncp, row, rflag, im)

pointer	tp		# table pointer
pointer	cp		# column pointer array
int	ncp		# size of column pointer array
int	row		# row where to begin insertion
bool	rflag		# use row number in header ?
pointer	im		# image pointer
#--
pointer	sp, colname, cn, duma
int	i, dumi
bool	match

errchk	tm_header, tm_copy

bool	streq()

begin
	call smark (sp)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (cn, SZ_COLNAME, TY_CHAR)
	call salloc (duma, max(SZ_COLUNITS,SZ_COLFMT),TY_CHAR)

	# Get column name from image header.
	call tm_header (im, Memc[colname], Memc[duma], Memc[duma])

	# Loop over table columns.
	match = false
	do i = 1, ncp {

	    # Get column name from table.
	    call tbcinf (Memi[cp+i-1], dumi, Memc[cn], Memc[duma], 
                        Memc[duma], dumi, dumi, dumi)

	    # Copy array if names match.
	    if (streq (Memc[colname], Memc[cn])) {
	        call tm_copy (tp, Memi[cp+i-1], row, rflag, im)
	        match = true
	    }
	}
	if (!match)
	    call error (1, "No column matched.")

	call sfree (sp)
end
