include "linmatch.h"

# RG_LWREC -- Procedure to write out the entire record.

procedure rg_lwrec (db, dformat, ls)

pointer	db			#I pointer to the database file
int	dformat			#I is the scaling file in database format
pointer	ls			#I pointer to the linmatch structure

pointer	sp, image
real	rg_lstatr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	if (dformat == YES) {
	    call rg_ldbparams (db, ls)
	    call rg_lwreg (db, ls)
	    call rg_ldbtscale (db, ls)
	} else {
	    call rg_lstats (ls, IMAGE, Memc[image], SZ_FNAME)
	    call fprintf (db, "%s  %g %g  %g %g")
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

int	i, nregions, rc1, rc2, rl1, rl2, c1, c2, l1, l2, del
real	xshift, yshift, bscale, bzero, bserr, bzerr
int	rg_lstati()
pointer	rg_lstatp()
real	rg_lstatr()

begin
	xshift = rg_lstatr (ls, SXSHIFT)
	yshift = rg_lstatr (ls, SYSHIFT)

	nregions = rg_lstati (ls, NREGIONS)
	do i = 1, nregions  {

	    rc1 = Memi[rg_lstatp (ls, RC1)+i-1]
	    rc2 = Memi[rg_lstatp (ls, RC2)+i-1]
	    rl1 = Memi[rg_lstatp (ls, RL1)+i-1]
	    rl2 = Memi[rg_lstatp (ls, RL2)+i-1]
	    if (IS_INDEFI(rc1))
		c1 = INDEFI
	    else
	        c1 = rc1 + xshift
	    if (IS_INDEFI(rc2))
		c2 = INDEFI
	    else
	        c2 = rc2 + xshift
	    if (IS_INDEFI(rl1))
		l1 = INDEFI
	    else
	        l1 = rl1 + yshift
	    if (IS_INDEFI(rl2))
		l2 = INDEFI
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

pointer	sp, str
int	rg_lstati()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

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
	    call pargi (rg_lstati(ls, NREGIONS))

	call sfree (sp)
end


# RG_LDBSCALER -- Write the scaling parameters for each region

procedure rg_ldbscaler (db, rc1, rc2, rl1, rl2, c1, c2, l1, l2, bscale,
	bzero, bserr, bzerr, del)

pointer	db		# pointer to the database file
int	rc1, rc2	# reference image region column limits
int	rl1, rl2	# reference image region line limits
int	c1, c2		# image region column limits
int	l1, l2		# image region line limits
real	bscale 		# the scaling parameter
real	bzero 		# the offset parameter
real	bserr		# the error in the scaling parameter
real	bzerr		# the error in the offset parameter
int	del		# the deletions index

begin
	if (IS_INDEFI(rc1) || IS_INDEFI(c1)) {
	    call dtput (db,"\t[INDEF] [INDEF]  %g %g  %g %g  %s\n")
	} else {
	    call dtput (db,"\t[%d:%d,%d:%d] [%d:%d,%d:%d]  %g %g  %g %g  %s\n")
	        call pargi (rc1)
	        call pargi (rc2)
	        call pargi (rl1)
	        call pargi (rl2)
	        call pargi (c1)
	        call pargi (c2)
	        call pargi (l1)
	        call pargi (l2)
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
int	i		#I the current region

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
	        call pargi (i)
		call pargi (Memi[rg_lstatp(ls,RC1)+i-1])
		call pargi (Memi[rg_lstatp(ls,RC2)+i-1])
		call pargi (Memi[rg_lstatp(ls,RL1)+i-1])
		call pargi (Memi[rg_lstatp(ls,RL2)+i-1])
		call pargr (Memr[rg_lstatp(ls,RBSCALE)+i-1])
		call pargr (Memr[rg_lstatp(ls,RBSCALEERR)+i-1])
		call pargr (Memr[rg_lstatp(ls,RBZERO)+i-1])
		call pargr (Memr[rg_lstatp(ls,RBZEROERR)+i-1])
	}
end
