include "linmatch.h"

# RG_LWREC -- Procedure to write out the entire record.

procedure rg_lwrec (db, dformat, ls)

pointer	db			#I pointer to the database file
int	dformat			#I is the scaling file in database format
pointer	ls			#I pointer to the linmatch structure

size_t	sz_val
int	i_val
pointer	sp, image
real	rg_lstatr()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)

	if (dformat == YES) {
	    call rg_ldbparams (db, ls)
	    call rg_lwreg (db, ls)
	    call rg_ldbtscale (db, ls)
	} else {
	    call rg_lstats (ls, IMAGE, Memc[image], SZ_FNAME)
	    i_val = db
	    call fprintf (i_val, "%s  %g %g  %g %g")
		call pargstr (Memc[image])
		call pargr (rg_lstatr(ls, TBSCALE))
		call pargr (rg_lstatr(ls, TBZERO))
		call pargr (rg_lstatr(ls, TBSCALEERR))
		call pargr (rg_lstatr(ls, TBZEROERR))
	}

	call sfree (sp)
end


# RG_LWREG -- Write out the results for each region.

procedure rg_lwreg (db, ls)

pointer	db		#I pointer to the database file
pointer	ls		#I pointer to the intensity matching structure

long	i
size_t	nregions
int	del
long	rc1, rc2, rl1, rl2, c1, c2, l1, l2
real	xshift, yshift, bscale, bzero, bserr, bzerr
long	rg_lstatl()
pointer	rg_lstatp()
real	rg_lstatr()

begin
	xshift = rg_lstatr (ls, SXSHIFT)
	yshift = rg_lstatr (ls, SYSHIFT)

	nregions = rg_lstatl (ls, NREGIONS)
	do i = 1, nregions  {

	    rc1 = Meml[rg_lstatp (ls, RC1)+i-1]
	    rc2 = Meml[rg_lstatp (ls, RC2)+i-1]
	    rl1 = Meml[rg_lstatp (ls, RL1)+i-1]
	    rl2 = Meml[rg_lstatp (ls, RL2)+i-1]
	    if (IS_INDEFL(rc1))
		c1 = INDEFL
	    else
	        c1 = rc1 + xshift
	    if (IS_INDEFL(rc2))
		c2 = INDEFL
	    else
	        c2 = rc2 + xshift
	    if (IS_INDEFL(rl1))
		l1 = INDEFL
	    else
	        l1 = rl1 + yshift
	    if (IS_INDEFL(rl2))
		l2 = INDEFL
	    else
	        l2 = rl2 + yshift

	    bscale = Memr[rg_lstatp(ls,RBSCALE)+i-1]
	    bzero = Memr[rg_lstatp(ls,RBZERO)+i-1]
	    bserr = Memr[rg_lstatp(ls,RBSCALEERR)+i-1]
	    bzerr = Memr[rg_lstatp(ls,RBZEROERR)+i-1]
	    del = Memi[rg_lstatp(ls,RDELETE)+i-1]

	    call rg_ldbscaler (db, rc1, rc2, rl1, rl2, c1, c2, l1, l2,
		bscale, bzero, bserr, bzerr, del)
	}
end


# RG_LDBPARAMS -- Write the intensity matching parameters to the database file.

procedure rg_ldbparams (db, ls)

pointer	db		#I pointer to the database file
pointer	ls		#I pointer to the intensity matching structure

size_t	sz_val
pointer	sp, str
long	rg_lstatl()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (str, sz_val, TY_CHAR)

	# Write out the time record was written.
	call dtput (db, "\n")
	call dtptime (db)

	# Write out the record name.
	call rg_lstats (ls, RECORD, Memc[str], SZ_FNAME)
	call dtput (db, "begin\t%s\n")
	    call pargstr (Memc[str])

	# Write the image names.
	call rg_lstats (ls, IMAGE, Memc[str], SZ_FNAME)
	call dtput (db, "\t%s\t\t%s\n")
	    call pargstr (KY_IMAGE)
	    call pargstr (Memc[str])
	call rg_lstats (ls, REFIMAGE, Memc[str], SZ_FNAME)
	call dtput (db, "\t%s\t%s\n")
	    call pargstr (KY_REFIMAGE)
	    call pargstr (Memc[str])

	call dtput (db, "\t%s\t%d\n")
	    call pargstr (KY_NREGIONS)
	    call pargl (rg_lstatl(ls, NREGIONS))

	call sfree (sp)
end


# RG_LDBSCALER -- Write the scaling parameters for each region

procedure rg_ldbscaler (db, rc1, rc2, rl1, rl2, c1, c2, l1, l2, bscale,
	bzero, bserr, bzerr, del)

pointer	db		# pointer to the database file
long	rc1, rc2	# reference image region column limits
long	rl1, rl2	# reference image region line limits
long	c1, c2		# image region column limits
long	l1, l2		# image region line limits
real	bscale 		# the scaling parameter
real	bzero 		# the offset parameter
real	bserr		# the error in the scaling parameter
real	bzerr		# the error in the offset parameter
int	del		# the deletions index

begin
	if (IS_INDEFL(rc1) || IS_INDEFL(c1)) {
	    call dtput (db,"\t[INDEF] [INDEF]  %g %g  %g %g  %s\n")
	} else {
	    call dtput (db,"\t[%d:%d,%d:%d] [%d:%d,%d:%d]  %g %g  %g %g  %s\n")
	        call pargl (rc1)
	        call pargl (rc2)
	        call pargl (rl1)
	        call pargl (rl2)
	        call pargl (c1)
	        call pargl (c2)
	        call pargl (l1)
	        call pargl (l2)
	}

	    call pargr (bscale)
	    call pargr (bzero)
	    call pargr (bserr)
	    call pargr (bzerr)
	    if (del == NO)
		call pargstr ("")
	    else
		call pargstr ("[Rejected/Deleted]")
end


# RG_LDBTSCALE -- Write the final scaling parameters and their errors.

procedure rg_ldbtscale (db, ls)

pointer	db		#I pointer to the text database file
pointer	ls		#I pointer to the linmatch structure

real	rg_lstatr()

begin
	call dtput (db, "\tbscale\t\t%g\n")
	    call pargr (rg_lstatr(ls, TBSCALE))
	call dtput (db, "\tbzero\t\t%g\n")
	    call pargr (rg_lstatr (ls, TBZERO))
	call dtput (db, "\tbserr\t\t%g\n")
	    call pargr (rg_lstatr (ls, TBSCALEERR))
	call dtput (db, "\tbzerr\t\t%g\n")
	    call pargr (rg_lstatr (ls, TBZEROERR))
end


# RG_LPWREC -- Print the computed scaling factors for the region.

procedure rg_lpwrec (ls, i)

pointer	ls		#I pointer to the linmatch structure
long	i		#I the current region

pointer rg_lstatp()
real	rg_lstatr()

begin
	if (i == 0) {
	    call printf (
	    "Results: bscale = %g +/- %g bzero = %g +/- %g\n")
		call pargr (rg_lstatr (ls, TBSCALE))
		call pargr (rg_lstatr (ls, TBSCALEERR))
		call pargr (rg_lstatr (ls, TBZERO))
		call pargr (rg_lstatr (ls, TBZEROERR))
	} else {
	    call printf (
	    "Region %d: [%d:%d,%d:%d] bscale = %g +/- %g bzero = %g +/- %g\n")
	        call pargl (i)
		call pargl (Meml[rg_lstatp(ls,RC1)+i-1])
		call pargl (Meml[rg_lstatp(ls,RC2)+i-1])
		call pargl (Meml[rg_lstatp(ls,RL1)+i-1])
		call pargl (Meml[rg_lstatp(ls,RL2)+i-1])
		call pargr (Memr[rg_lstatp(ls,RBSCALE)+i-1])
		call pargr (Memr[rg_lstatp(ls,RBSCALEERR)+i-1])
		call pargr (Memr[rg_lstatp(ls,RBZERO)+i-1])
		call pargr (Memr[rg_lstatp(ls,RBZEROERR)+i-1])
	}
end
