include	<math/curfit.h>
include	"identify.h"

# ID_DBREAD -- Read features data from the database.

procedure id_dbread (id, name, verbose)

pointer	id				# ID pointer
char	name[SZ_LINE]
int	verbose

pointer	dt
int	i, j, ncoeffs, rec
pointer	sp, coeffs, line

int	dtgeti(), dcvstati(), dtlocate(), dtscan(), nscan()
real	dtgetr()
double	dcvstatd()
pointer	dtmap1()

errchk	dtmap1(), dtlocate(), dtgeti(), dtgad()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	call strcpy ("id", Memc[line], SZ_LINE)
	call imgcluster (name, Memc[line+2], SZ_LINE)
	dt = dtmap1 (Memc[ID_DATABASE(id)], Memc[line], READ_ONLY)

	call sprintf (Memc[line], SZ_LINE, "identify %s")
	    call pargstr (name)

	rec = dtlocate (dt, Memc[line])
	if (rec == EOF)
	    call error (0, "Entry not found")

	i = dtgeti (dt, rec, "features")

	ID_NALLOC(id) = i
	ID_NFEATURES(id) = i
	ID_CURRENT(id) = 0
	call realloc (ID_PIX(id), i, TY_DOUBLE)
	call realloc (ID_FIT(id), i, TY_DOUBLE)
	call realloc (ID_USER(id), i, TY_DOUBLE)
	call realloc (ID_WTS(id), i, TY_DOUBLE)
	call realloc (ID_FWIDTHS(id), i, TY_REAL)
	call realloc (ID_FTYPES(id), i, TY_INT)

	do i = 1, ID_NFEATURES(id) {
	    j = dtscan (dt)
	    call gargd (PIX(id,i))
	    call gargd (FIT(id,i))
	    call gargd (USER(id,i))
	    call gargr (FWIDTH(id,i))
	    call gargi (FTYPE(id,i))
	    call gargd (WTS(id,i))

	    # The following initialization is for backwards compatibility.
	    if (nscan() < 5) {
	        FWIDTH(id,i) = ID_FWIDTH(id)
	        FTYPE(id,i) = ID_FTYPE(id)
	    } else if (nscan() < 6)
		WTS(id,i) = 1.

	}

	iferr (ID_SHIFT(id) = dtgetr (dt, rec, "shift"))
	    ID_SHIFT(id) = 0.

	iferr {
	    ncoeffs = dtgeti (dt, rec, "coefficients")
	    call salloc (coeffs, ncoeffs, TY_DOUBLE)
	    call dtgad (dt, rec, "coefficients", Memd[coeffs], ncoeffs, ncoeffs)

	    if (ID_CV(id) != NULL)
		call dcvfree (ID_CV(id))
	    call dcvrestore (ID_CV(id), Memd[coeffs])

	    call ic_putr (ID_IC(id), "xmin", real (dcvstatd(ID_CV(id), CVXMIN)))
	    call ic_putr (ID_IC(id), "xmax", real (dcvstatd(ID_CV(id), CVXMAX)))
	    ifnoerr (call dtgstr (dt, rec, "function", Memc[line], SZ_LINE)) {
	        call ic_pstr (ID_IC(id), "function", Memc[line])
	        call ic_puti (ID_IC(id), "order", dtgeti (dt, rec, "order"))
	        call dtgstr (dt, rec, "sample", Memc[line], SZ_LINE)
	        call ic_pstr (ID_IC(id), "sample", Memc[line])
	        call ic_puti (ID_IC(id), "naverage",
		    dtgeti (dt, rec, "naverage"))
	        call ic_puti (ID_IC(id), "niterate",
		    dtgeti (dt, rec, "niterate"))
	        call ic_putr (ID_IC(id), "low", dtgetr (dt, rec, "low_reject"))
	        call ic_putr (ID_IC(id), "high",
		    dtgetr (dt, rec, "high_reject"))
	        call ic_putr (ID_IC(id), "grow", dtgetr (dt, rec, "grow"))
	    } else {
	        call ic_puti (ID_IC(id), "order", dcvstati (ID_CV(id), CVORDER))
	        switch (dcvstati (ID_CV(id), CVTYPE)) {
	        case LEGENDRE:
	            call ic_pstr (ID_IC(id), "function", "legendre")
	        case CHEBYSHEV:
	            call ic_pstr (ID_IC(id), "function", "chebyshev")
	        case SPLINE1:
	            call ic_pstr (ID_IC(id), "function", "spline1")
	        case SPLINE3:
	            call ic_pstr (ID_IC(id), "function", "spline3")
		}
	    }

	    ID_NEWCV(id) = YES
	    ID_CURRENT(id) = min (1, ID_NFEATURES(id))
	} then
		;

	call dtunmap (dt)
	call sfree (sp)

	if (ID_NFEATURES(id) > 0) {
	    ID_NEWGRAPH(id) = YES
	    ID_NEWFEATURES(id) = YES
	    ID_CURRENT(id) = 1
	} else
	    ID_CURRENT(id) = 0

	if (verbose == YES) {
	    call printf ("identify %s\n")
		call pargstr (name)
	}
end


# ID_DBWRITE -- Write features data to the database.

procedure id_dbwrite (id, name, verbose)

pointer	id				# ID pointer
char	name[ARB]
int	verbose

int	i, ncoeffs
pointer	dt, sp, coeffs, root

int	dcvstati(), ic_geti()
real	ic_getr()
pointer	dtmap1(), immap()

errchk	dtmap1, immap

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)

	call strcpy ("id", Memc[root], SZ_FNAME)
	call imgcluster (name, Memc[root+2], SZ_FNAME)
	dt = dtmap1 (Memc[ID_DATABASE(id)], Memc[root], APPEND)

	call dtptime (dt)
	call dtput (dt, "begin\tidentify %s\n")
	    call pargstr (name)
	call dtput (dt, "\tid\t%s\n")
	    call pargstr (name)
	call dtput (dt, "\ttask\tidentify\n")
	call dtput (dt, "\timage\t%s\n")
	    call pargstr (Memc[ID_IMAGE(id)])

	call dtput (dt, "\tfeatures\t%d\n")
	    call pargi (ID_NFEATURES(id))
	do i = 1, ID_NFEATURES(id) {
	    call dtput (dt, "\t\t%10.2f\t%10.8g\t%10.8g\t%5.1f %d %d\n")
		call pargd (PIX(id,i))
		call pargd (FIT(id,i))
		call pargd (USER(id,i))
		call pargr (FWIDTH(id,i))
		call pargi (FTYPE(id,i))
		call pargd (WTS(id,i))
	}

	if (ID_SHIFT(id) != 0.) {
	    call dtput (dt, "\tshift\t%g\n")
		call pargd (ID_SHIFT(id))
	}

	if (ID_CV(id) != NULL) {
	    call dtput (dt, "\tfunction %s\n")
		call ic_gstr (ID_IC(id), "function", Memc[root], SZ_FNAME)
		call pargstr (Memc[root])
	    call dtput (dt, "\torder %d\n")
		call pargi (ic_geti (ID_IC(id), "order"))
	    call dtput (dt, "\tsample %s\n")
		call ic_gstr (ID_IC(id), "sample", Memc[root], SZ_FNAME)
		call pargstr (Memc[root])
	    call dtput (dt, "\tnaverage %d\n")
		call  pargi (ic_geti (ID_IC(id), "naverage"))
	    call dtput (dt, "\tniterate %d\n")
		call  pargi (ic_geti (ID_IC(id), "niterate"))
	    call dtput (dt, "\tlow_reject %g\n")
		call  pargr (ic_getr (ID_IC(id), "low"))
	    call dtput (dt, "\thigh_reject %g\n")
		call  pargr (ic_getr (ID_IC(id), "high"))
	    call dtput (dt, "\tgrow %g\n")
		call  pargr (ic_getr (ID_IC(id), "grow"))

	    ncoeffs = dcvstati (ID_CV(id), CVNSAVE)
	    call salloc (coeffs, ncoeffs, TY_DOUBLE)
	    call dcvsave (ID_CV(id), Memd[coeffs])
	    call dtput (dt, "\tcoefficients\t%d\n")
		call pargi (ncoeffs)
	    do i = 1, ncoeffs {
		call dtput (dt, "\t\t%g\n")
		    call pargd (Memd[coeffs+i-1])
	    }
	}

	call dtput (dt, "\n")
	call dtunmap (dt)

	ID_NEWFEATURES(id) = NO
	ID_NEWCV(id) = NO
	ID_NEWDBENTRY(id) = NO

	if (verbose == YES) {
	    call printf ("identify %s\n")
		call pargstr (name)
	}

	# Enter reference spectrum name in image header.
	dt = immap (Memc[ID_IMAGE(id)], READ_WRITE, 0)
	call imgcluster (Memc[ID_IMAGE(id)], Memc[root], SZ_FNAME)
	call imastr (dt, "REFSPEC1", Memc[root])
	iferr (call imdelf (dt, "REFSPEC2"))
	    ;
	call imunmap (dt)
 
	call sfree (sp)
end
