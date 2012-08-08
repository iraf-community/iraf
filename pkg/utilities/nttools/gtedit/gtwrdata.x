include	<mach.h>
include	<gset.h>
include	<tbset.h>

# GT_WRDATA -- Write out the complete table record for this point

procedure gt_wrdata (gd, tp, wx, wy, x, y, npix)

pointer	gd				# Graphics  descriptor
pointer	tp				# Table  descriptor
real	wx				# Cursor position
real	wy				#   ""
real	x[ARB]				# Plotted data
real	y[ARB]				# Plotted data
int	npix				# # of pixels

pointer	sp
pointer cname, cunits, cfmt	# pointers to scratch space for column info
pointer	ctext, cp
int	row, i, colnum, datatype, lendata, lenfmt, ncols
int	ip
real	r2min, r2, x0, y0

pointer tbpsta(), tbcnum()

begin
	# Allocate some space
	call smark (sp)
	call salloc (cname, SZ_LINE, TY_CHAR)
	call salloc (cunits, SZ_LINE, TY_CHAR)
	call salloc (cfmt, SZ_COLFMT, TY_CHAR)
	call salloc (ctext, SZ_LINE, TY_CHAR)

	# Search for the nearest point
	row = 0
	r2min = MAX_REAL

	# Transform  world cursor coordintes to NDC
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	do i = 1 , npix {
	    call gctran (gd, x[i], y[i], x0, y0, 1, 0)
	    if (x[i] < INDEFR && y[i] < INDEFR)
	        r2 = (wx - x0) ** 2 + (wy - y0) ** 2
	    else
		r2 = MAX_REAL

	    if (r2 < r2min) {
		r2min = r2
		row = i
	    }
	}

	if (row != 0) {
	    # Deactivate the workstation
	    call gdeactivate (gd, 0)
	    # Now get the info on the columns
	    ncols = tbpsta (tp, TBL_NCOLS)

	    call printf ("\n")
	    do i = 1, ncols {
		cp = tbcnum (tp, i)
	        call tbcinf (cp,
		    colnum, Memc[cname], Memc[cunits], Memc[cfmt],
		    datatype, lendata, lenfmt)

		# Print column units (ignore trailing blanks)
		# (calling sequence of inquotes modified by PEH on 13 Jan 1995)
		call inquotes (Memc[cunits], Memc[cunits], SZ_LINE, NO)
		call printf (" %-14s ")
		    call pargstr (Memc[cunits])

		# Print column name (and include trailing blanks)
	    	call inquotes (Memc[cname], Memc[cname], SZ_LINE, YES)
		call printf ("%-16s ")
		    call pargstr (Memc[cname])

		#Print column value
		# Modified by by PEH, 9 Sept 1994:
		#     remove case statement, and skip leading blanks.
		call tbegtt (tp, cp, row, Memc[ctext], SZ_LINE)
		ip = 0
		while (Memc[ctext+ip] == ' ')
		    ip = ip + 1
		call printf ("%s\n")
		    call pargstr (Memc[ctext+ip])
	    }
	}
	call greactivate (gd, AW_PAUSE)
	call sfree (sp)
end
