include	<gset.h>
include	<tbset.h>

# GT_WRHEAD -- Write out the column names of the table

procedure gt_wrhead (gd, tp)

pointer	gd				# Graphics  descriptor
pointer	tp				# Table  descriptor

pointer	sp
pointer cname, cunits, cfmt	# pointers to scratch space for column info
pointer	ctext, cp
int	i, colnum, datatype, lendata, lenfmt, ncols

pointer tbpsta(), tbcnum()

begin
	# Allocate some space
	call smark (sp)
	call salloc (cname, SZ_LINE, TY_CHAR)
	call salloc (cunits, SZ_LINE, TY_CHAR)
	call salloc (cfmt, SZ_COLFMT, TY_CHAR)
	call salloc (ctext, SZ_LINE, TY_CHAR)

	# Deactivate the workstation
	call gdeactivate (gd, 0)
	# Now get the info on the columns
	ncols = tbpsta (tp, TBL_NCOLS)

	call printf ("Column names:\n\n")
	do i = 1, ncols {
	    cp = tbcnum (tp, i)
	    call tbcinf (cp,
	        colnum, Memc[cname], Memc[cunits], Memc[cfmt],
		datatype, lendata, lenfmt)

	    # Print column name (and include trailing blanks)
	    # (calling sequence of inquotes modified by PEH on 13 Jan 1995)
	    call inquotes (Memc[cname], Memc[cname], SZ_LINE, YES)
	    call printf ("%-16s \n")
	        call pargstr (Memc[cname])

	}
	call greactivate (gd, AW_PAUSE)
	call sfree (sp)
end
