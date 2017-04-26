include <gset.h>
include <tbset.h>
include "../lib/apseldef.h"
include "../lib/psfdef.h"


# DP_RPSTARS -- Read in the IDS and x and y positions of the PSF stars.

procedure dp_rpstars (dao, im, pst, text_file, gd, mgd, id, mkstars,
	matchbyid, showplots)

pointer	dao		# pointer to the daophot structure
pointer	im		# the input image descriptor
int	pst		# the psf star list file descriptor
bool	text_file	# text or table file ?
pointer	gd		# the graphics descriptor
pointer	mgd		# the plot file descriptor
pointer	id		# the display device descriptor
bool	mkstars		# mark the stars added to the psf
bool	matchbyid	# match psf stars by id or position
bool	showplots	# show the psf star plots

real	x, y, mag, rjunk
pointer	sp, fields, indices, key
int	i, nrow, idno
real	dp_pstatr()
int	tbpsta(), dp_apsel(), dp_addstar()

begin
	call smark (sp)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (indices, PSF_NINCOLS, TY_INT) 

	if (text_file) {
	    call pt_kyinit (key)
	    Memi[indices] = DP_PAPID
	    Memi[indices+1] = DP_PAPXCEN
	    Memi[indices+2] = DP_PAPYCEN
	    Memi[indices+3] = DP_PAPMAG1
	    call dp_gappsf (Memi[indices], Memc[fields], PSF_NINCOLS)
	} else {
	    call dp_tptinit (pst, Memi[indices])
	    nrow = tbpsta (pst, TBL_NROWS)
	}

	i = 1
	repeat {

	    # Read the next star.

	    if (text_file) {
		if (dp_apsel (key, pst, Memc[fields], Memi[indices], idno,
		    x, y, rjunk, mag) == EOF)
		    break
	    } else {
		if (i > nrow)
		    break
		call dp_tptread (pst, Memi[indices], idno, x, y, mag, i)
	    }

	    call dp_win (dao, im, x, y, x, y, 1)

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

int	pst		# the psf star list file descriptor
int	column[ARB]	# array of column pointers

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
int	column[ARB]	# array of column pointers
int	idno		# the output id number
real	x, y		# the output x and y position
real	mag		# the output magnitude
int	rowno		# the row number

bool	nullflag

begin
	call tbrgti (pst, column[1], idno, nullflag, 1, rowno)
	if (nullflag)
	    idno = 0
	call tbrgtr (pst, column[2], x, nullflag, 1, rowno)
	call tbrgtr (pst, column[3], y, nullflag, 1, rowno)
	call tbrgtr (pst, column[4], mag, nullflag, 1, rowno)
end
