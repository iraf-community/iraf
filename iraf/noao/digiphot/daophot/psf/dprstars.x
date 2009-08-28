include <gset.h>
include <tbset.h>
include "../lib/apseldef.h"
include "../lib/psfdef.h"


# DP_RPSTARS -- Read in the IDS and x and y positions of the PSF stars.

procedure dp_rpstars (dao, im, pst, text_file, gd, mgd, id, mkstars,
	matchbyid, showplots)

pointer	dao		# pointer to the daophot structure
pointer	im		# the input image descriptor
pointer	pst		# the psf star list file descriptor
bool	text_file	# text or table file ?
pointer	gd		# the graphics descriptor
pointer	mgd		# the plot file descriptor
pointer	id		# the display device descriptor
bool	mkstars		# mark the stars added to the psf
bool	matchbyid	# match psf stars by id or position
bool	showplots	# show the psf star plots

size_t	sz_val
real	x, y, mag, rjunk
pointer	sp, fields, key, indices, p_indices
long	i, nrow
int	idno, i_val
real	dp_pstatr()
int	dp_apsel(), dp_addstar()
long	tbpstl()

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (fields, sz_val, TY_CHAR)
	sz_val = PSF_NINCOLS
	if (text_file) {
	    call salloc (indices, sz_val, TY_INT) 
	} else {
	    call salloc (p_indices, sz_val, TY_POINTER)
	}

	if (text_file) {
	    call pt_kyinit (key)
	    Memi[indices] = DP_PAPID
	    Memi[indices+1] = DP_PAPXCEN
	    Memi[indices+2] = DP_PAPYCEN
	    Memi[indices+3] = DP_PAPMAG1
	    call dp_gappsf (Memi[indices], Memc[fields], PSF_NINCOLS)
	} else {
	    call dp_tptinit (pst, Memp[p_indices])
	    nrow = tbpstl (pst, TBL_NROWS)
	}

	i = 1
	repeat {

	    # Read the next star.

	    if (text_file) {
		i_val = pst
		if (dp_apsel (key, i_val, Memc[fields], Memi[indices], idno,
		    x, y, rjunk, mag) == EOF)
		    break
	    } else {
		if (i > nrow)
		    break
		call dp_tptread (pst, Memp[p_indices], idno, x, y, mag, i)
	    }

	    sz_val = 1
	    call dp_win (dao, im, x, y, x, y, sz_val)

	    # Add it to the PSF star list.
	    if (idno > 0) {
		if (matchbyid) {
	            if (dp_addstar (dao, im, x, y, mag, idno, gd, mgd,
		        showplots) == OK) {
			if (mkstars && id != NULL) {
			    call gmark (id, dp_pstatr(dao, CUR_PSFX),
			        dp_pstatr(dao, CUR_PSFY), GM_PLUS, -5.0, -5.0)
			    if (id == gd)
				call gflush (id)
			    else
				call gframe (id)
			}
			
		    }
		} else {
	            if (dp_addstar (dao, im, x, y, INDEFR, 0, gd, mgd,
		        showplots) == OK) {
			if (mkstars && id != NULL) {
			    call gmark (id, dp_pstatr(dao, CUR_PSFX),
			        dp_pstatr(dao, CUR_PSFY), GM_PLUS, -5.0, -5.0)
			    if (id == gd)
				call gflush (id)
			    else
				call gframe (id)
			}
		    }
		}
	    }

	    i = i + 1
	}

	if (text_file)
	    call pt_kyfree (key)
	call sfree (sp)
end


# DP_TPTINIT -- Set up the input psf star list ST table column pointers.

procedure dp_tptinit (pst, column)

pointer	pst		# the psf star list file descriptor
pointer	column[ARB]	# array of column pointers

begin
	call tbcfnd (pst, ID, column[1], 1)
	if (column[1] == NULL)
	    call tbcfnd (pst, "ID", column[1], 1)
	if (column[1] == NULL)
	    call error (0, "Error reading ID column from PSF star file\n")

	call tbcfnd (pst, XCENTER, column[2], 1)
	if (column[2] == NULL)
	    call tbcfnd (pst, "XCENTER", column[2], 1)
	if (column[2] == NULL)
	    call error (0, "Error reading XCENTER column from PSF star file\n")

	call tbcfnd (pst, YCENTER, column[3], 1)
	if (column[3] == NULL)
	    call tbcfnd (pst, "YCENTER", column[3], 1)
	if (column[3] == NULL)
	    call error (0, "Error reading YCENTER column from PSF star file\n")

	call tbcfnd (pst, MAG, column[4], 1)
	if (column[4] == NULL)
	    call tbcfnd (pst, APMAG, column[4], 1)
	if (column[4] == NULL)
	    call error (0, "Error reading MAG column from PSF star file\n")
end


# DP_TPTREAD -- Read the id from an ST table.

procedure dp_tptread (pst, column, idno, x, y, mag, rowno)

pointer	pst		# pointer to the ST table
pointer	column[ARB]	# array of column pointers
int	idno		# the output id number
real	x, y		# the output x and y position
real	mag		# the output magnitude
long	rowno		# the row number

bool	nullflag

begin
	call tbrgti (pst, column[1], idno, nullflag, 1, rowno)
	if (nullflag)
	    idno = 0
	call tbrgtr (pst, column[2], x, nullflag, 1, rowno)
	call tbrgtr (pst, column[3], y, nullflag, 1, rowno)
	call tbrgtr (pst, column[4], mag, nullflag, 1, rowno)
end
