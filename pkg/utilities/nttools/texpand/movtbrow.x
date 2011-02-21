include	<tbset.h>

# MOVTBROW -- Move columns from one table to another where not null
#
# B.Simon	25-Apr-88	Original
# B.Simon	15-Jan-99	now calls mov_elem

procedure movtbrow (rtp, rrow, wtp, wrow)

pointer	rtp		# i: Table descriptor of table read from
int	rrow		# i: Row number of table read from
pointer	wtp		# i: Table descriptor of table written to
int	wrow		# i: Row number of table written to
#--
int	ncol, icol
pointer	sp, rcp, wcp, colname

pointer	tbpsta(), tbcnum()

begin
	call smark (sp)
	call salloc (colname, SZ_COLNAME, TY_CHAR)

	ncol = tbpsta (rtp, TBL_NCOLS)
	do icol = 1, ncol {

	    rcp = tbcnum (rtp, icol)
	    call tbcigt (rcp, TBL_COL_NAME, Memc[colname], SZ_COLNAME)
	    call tbcfnd (wtp, Memc[colname], wcp, 1)

#	    Column names beginning with an underscore are for internal
#	    use by the program and do not contain actual data

	    if (Memc[colname] != '_' && wcp != NULL) {

		# Copy the row and column in its native type

		call mov_elem (rtp, rcp, rrow, wtp, wcp, wrow)
	    }
	}

	call sfree (sp)
end
