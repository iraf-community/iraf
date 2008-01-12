include	<imset.h>
include	<math/curfit.h>
include	<smw.h>
include	"identify.h"

# ID_DBREAD -- Read features data from the database.

procedure id_dbread (id, name, ap, add, verbose)

pointer	id				# ID pointer
char	name[SZ_LINE]			# Image name
int	ap[2]				# Aperture number
int	add				# Add features?
int	verbose				# Verbose flag

double	pix
int	i, j, k, ncoeffs, rec
pointer	dt, sp, coeffs, line, str, sh

int	dtgeti(), dcvstati(), dtlocate(), dtscan(), nscan()
real	dtgetr()
double	dcvstatd()

errchk	dtremap(), dtlocate(), dtgeti(), dtgad()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	call strcpy ("id", Memc[line], SZ_LINE)
	call imgcluster (name, Memc[line+2], SZ_LINE)
	call dtremap (ID_DT(id), Memc[ID_DATABASE(id)], Memc[line], READ_ONLY)

	call id_dbsection (id, name, ap, Memc[ID_SECTION(id)], SZ_FNAME)
	call sprintf (Memc[line], SZ_LINE, "identify %s%s")
	    call pargstr (name)
	    call pargstr (Memc[ID_SECTION(id)])

	dt = ID_DT(id)
	sh = ID_SH(id)
	iferr (rec = dtlocate (dt, Memc[line])) {
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[str], SZ_LINE, "Entry not found: %s")
		call pargstr (Memc[line])
	    call error (0, Memc[str])
	}

	if (add == YES) {
	    j = dtgeti (dt, rec, "features")
	    k = j + ID_NFEATURES(id)

	    call realloc (ID_PIX(id), k, TY_DOUBLE)
	    call realloc (ID_FIT(id), k, TY_DOUBLE)
	    call realloc (ID_USER(id), k, TY_DOUBLE)
	    call realloc (ID_WTS(id), k, TY_DOUBLE)
	    call realloc (ID_FWIDTHS(id), k, TY_REAL)
	    call realloc (ID_FTYPES(id), k, TY_INT)
	    call realloc (ID_LABEL(id), k, TY_POINTER)

	    do i = 1, j {
		k = dtscan (dt)
		call gargd (pix)

		ID_NFEATURES(id) = ID_NFEATURES(id) + 1
		for (k=ID_NFEATURES(id); (k>1)&&(pix<PIX(id,k-1)); k=k-1) {
		    PIX(id,k) = PIX(id,k-1)
		    FIT(id,k) = FIT(id,k-1)
		    USER(id,k) = USER(id,k-1)
		    WTS(id,k) = WTS(id,k-1)
		    FWIDTH(id,k) = FWIDTH(id,k-1)
		    FTYPE(id,k) = FTYPE(id,k-1)
		    Memi[ID_LABEL(id)+k-1] = Memi[ID_LABEL(id)+k-2]
		}
		PIX(id,k) = pix
		call gargd (FIT(id,k))
		call gargd (USER(id,k))
		call gargr (FWIDTH(id,k))
		call gargi (FTYPE(id,k))
		call gargd (WTS(id,k))
		call gargstr (Memc[line], SZ_LINE)
		Memi[ID_LABEL(id)+k-1] = NULL
		call id_label (Memc[line], Memi[ID_LABEL(id)+k-1])

		# The following initialization is for backwards compatibility.
		if (nscan() < 5) {
		    FWIDTH(id,k) = ID_FWIDTH(id)
		    FTYPE(id,k) = ID_FTYPE(id)
		} else if (nscan() < 6)
		    WTS(id,k) = 1.
	    }

	} else {
	    if (SMW_FORMAT(MW(sh)) == SMW_ES || SMW_FORMAT(MW(sh)) == SMW_MS) {
		iferr (APLOW(sh,1) = dtgetr (dt, rec, "aplow"))
		    APLOW(sh,1) = INDEF
		iferr (APHIGH(sh,1) = dtgetr (dt, rec, "aphigh"))
		    APHIGH(sh,1) = INDEF
	    }

	    do i = 1, ID_NFEATURES(id)
		call mfree (Memi[ID_LABEL(id)+i-1], TY_CHAR)

	    k = dtgeti (dt, rec, "features")
	    ID_NFEATURES(id) = k
	    ID_NALLOC(id) = k
	    call realloc (ID_PIX(id), k, TY_DOUBLE)
	    call realloc (ID_FIT(id), k, TY_DOUBLE)
	    call realloc (ID_USER(id), k, TY_DOUBLE)
	    call realloc (ID_WTS(id), k, TY_DOUBLE)
	    call realloc (ID_FWIDTHS(id), k, TY_REAL)
	    call realloc (ID_FTYPES(id), k, TY_INT)
	    call realloc (ID_LABEL(id), k, TY_POINTER)

	    do i = 1, ID_NFEATURES(id) {
		k = dtscan (dt)
		call gargd (PIX(id,i))
		call gargd (FIT(id,i))
		call gargd (USER(id,i))
		call gargr (FWIDTH(id,i))
		call gargi (FTYPE(id,i))
		call gargd (WTS(id,i))
		call gargstr (Memc[line], SZ_LINE)
		Memi[ID_LABEL(id)+i-1] = NULL
		call id_label (Memc[line], Memi[ID_LABEL(id)+i-1])

		# The following initialization is for backwards compatibility.
		if (nscan() < 5) {
		    FWIDTH(id,i) = ID_FWIDTH(id)
		    FTYPE(id,i) = ID_FTYPE(id)
		} else if (nscan() < 6)
		    WTS(id,i) = 1.
	    }

	    iferr (ID_SHIFT(id) = dtgetr (dt, rec, "shift"))
		ID_SHIFT(id) = 0.
	    iferr (ID_REDSHIFT(id) = dtgetr (dt, rec, "redshift"))
		ID_REDSHIFT(id) = 0.
	    iferr (ID_RMSRED(id) = dtgetr (dt, rec, "redshift_rms"))
		ID_RMSRED(id) = 0.

	    iferr {
		ncoeffs = dtgeti (dt, rec, "coefficients")
		call salloc (coeffs, ncoeffs, TY_DOUBLE)
		call dtgad (dt, rec, "coefficients", Memd[coeffs], ncoeffs,
		    ncoeffs)

		if (ID_CV(id) != NULL)
		    call dcvfree (ID_CV(id))
		call dcvrestore (ID_CV(id), Memd[coeffs])

		call ic_putr (ID_IC(id), "xmin", real (dcvstatd(ID_CV(id),
		    CVXMIN)))
		call ic_putr (ID_IC(id), "xmax", real (dcvstatd(ID_CV(id),
		    CVXMAX)))
		ifnoerr (call dtgstr (dt,rec,"function",Memc[line],SZ_LINE)) {
		    call ic_pstr (ID_IC(id), "function", Memc[line])
		    call ic_puti (ID_IC(id), "order", dtgeti (dt, rec, "order"))
		    call dtgstr (dt, rec, "sample", Memc[line], SZ_LINE)
		    call ic_pstr (ID_IC(id), "sample", Memc[line])
		    call ic_puti (ID_IC(id), "naverage",
			dtgeti (dt, rec, "naverage"))
		    call ic_puti (ID_IC(id), "niterate",
			dtgeti (dt, rec, "niterate"))
		    call ic_putr (ID_IC(id), "low",
			dtgetr (dt, rec, "low_reject"))
		    call ic_putr (ID_IC(id), "high",
			dtgetr (dt, rec, "high_reject"))
		    call ic_putr (ID_IC(id), "grow", dtgetr (dt, rec, "grow"))
		} else {
		    call ic_puti (ID_IC(id), "order", dcvstati (ID_CV(id),
			CVORDER))
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
	}

	call sfree (sp)

	if (ID_NFEATURES(id) > 0) {
	    ID_NEWGRAPH(id) = YES
	    ID_NEWFEATURES(id) = YES
	    ID_CURRENT(id) = 1
	} else
	    ID_CURRENT(id) = 0

	if (verbose == YES) {
	    call printf ("identify %s%s\n")
		call pargstr (name)
		call pargstr (Memc[ID_SECTION(id)])
	}
end


# ID_DBWRITE -- Write features data to the database.

procedure id_dbwrite (id, name, ap, verbose)

pointer	id				# ID pointer
char	name[ARB]			# Image name
int	ap[2]				# Aperture number
int	verbose				# Verbose flag

int	i, ncoeffs
pointer	dt, sp, coeffs, root, sh, im

int	dcvstati(), ic_geti()
real	ic_getr()

errchk	dtremap

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)

	call strcpy ("id", Memc[root], SZ_FNAME)
	call imgcluster (name, Memc[root+2], SZ_FNAME)
	call dtremap (ID_DT(id), Memc[ID_DATABASE(id)], Memc[root], APPEND)

	call id_dbsection (id, name, ap, Memc[ID_SECTION(id)], SZ_FNAME)

	sh = ID_SH(id)
	dt = ID_DT(id)
	call dtptime (dt)
	call dtput (dt, "begin\tidentify %s%s\n")
	    call pargstr (name)
	    call pargstr (Memc[ID_SECTION(id)])
	call dtput (dt, "\tid\t%s\n")
	    call pargstr (name)
	call dtput (dt, "\ttask\tidentify\n")
	call dtput (dt, "\timage\t%s%s\n")
	    call pargstr (Memc[ID_IMAGE(id)])
	    call pargstr (Memc[ID_SECTION(id)])
	if (SMW_FORMAT(MW(sh)) == SMW_ES || SMW_FORMAT(MW(sh)) == SMW_MS) {
	    call dtput (dt, "\taperture\t%d\n")
		call pargi (ID_AP(id,1))
	    call dtput (dt, "\taplow\t%g\n")
		call pargr (APLOW(sh,1))
	    call dtput (dt, "\taphigh\t%g\n")
		call pargr (APHIGH(sh,1))
	}

	call dtput (dt, "\tfeatures\t%d\n")
	    call pargi (ID_NFEATURES(id))
	do i = 1, ID_NFEATURES(id) {
	    call dtput (dt, "\t    %10.2f %10.8g %10.8g %5.1f %d %d %s\n")
		call pargd (PIX(id,i))
		call pargd (FIT(id,i))
		call pargd (USER(id,i))
		call pargr (FWIDTH(id,i))
		call pargi (FTYPE(id,i))
		call pargd (WTS(id,i))
		if (Memi[ID_LABEL(id)+i-1] != NULL)
		    call pargstr (Memc[Memi[ID_LABEL(id)+i-1]])
		else
		    call pargstr ("")
	}

	if (ID_SHIFT(id) != 0.) {
	    call dtput (dt, "\tshift\t%g\n")
		call pargd (ID_SHIFT(id))
	}
	if (ID_REDSHIFT(id) != 0.) {
	    call dtput (dt, "\tredshift\t%g\n")
		call pargd (ID_REDSHIFT(id))
	    call dtput (dt, "\tredshift_rms\t%g\n")
		call pargd (ID_RMSRED(id))
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

	ID_NEWFEATURES(id) = NO
	ID_NEWCV(id) = NO
	ID_NEWDBENTRY(id) = NO

	if (ID_TASK(id) == IDENTIFY) {
	    if (verbose == YES) {
		call printf ("identify %s%s\n")
		    call pargstr (name)
		    call pargstr (Memc[ID_SECTION(id)])
	    }

	    # Enter reference spectrum name in image header.
	    im = IM(sh)
	    call imseti (im, IM_WHEADER, YES)
	    call imastr (im, "REFSPEC1", Memc[ID_IMAGE(id)])
	    iferr (call imdelf (im, "REFSPEC2"))
		;
	}
 
	call sfree (sp)
end


# ID_DBCHECK -- Check if there is an entry in the database.
# This does not actually read the database entry.  It also assumes that
# if a database is already open it is for the same image (the image
# names are not checked) and the database has been scanned.

int procedure id_dbcheck (id, name, ap)

pointer	id				# ID pointer
char	name[SZ_LINE]			# Image name
int	ap[2]				# Aperture number

int	rec, stat
pointer	sp, line, sec

int	dtlocate()

errchk	dtremap(), dtlocate()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (sec, SZ_LINE, TY_CHAR)

	if (ID_DT(id) == NULL) {
	    call strcpy ("id", Memc[line], SZ_LINE)
	    call imgcluster (name, Memc[line+2], SZ_LINE)
	    iferr (call dtremap (ID_DT(id), Memc[ID_DATABASE(id)], Memc[line],
		READ_ONLY)) {
		call sfree (sp)
		return (NO)
	    }
	}

	call id_dbsection (id, name, ap, Memc[sec], SZ_LINE)
	call sprintf (Memc[line], SZ_LINE, "identify %s%s")
	    call pargstr (name)
	    call pargstr (Memc[sec])

	iferr (rec = dtlocate (ID_DT(id), Memc[line]))
	    stat = NO
	else
	    stat = YES

	call sfree (sp)
	return (stat)
end


# ID_DBSECTION -- Make the IDENTIFY section.

procedure id_dbsection (id, name, ap, section, sz_section)

pointer	id				#I ID pointer
char	name[SZ_LINE]			#I Image name
int	ap[2]				#I Aperture number
char	section[sz_section]		#O IDENTIFY section
int	sz_section			#I Size of section string

pointer	sh, smw
bool	streq()

begin
	sh = ID_SH(id)
	smw = MW(sh)

	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    section[1] = EOS
	    if (streq (name, Memc[ID_IMAGE(id)])) {
		switch (SMW_LDIM(smw)) {
		case 2:
		    switch (SMW_LAXIS(smw,1)) {
		    case 1:
			call sprintf (section, sz_section, "[*,%d]")
		    case 2:
			call sprintf (section, sz_section, "[%d,*]")
		    }
		    #call pargi (LINDEX(sh,1))
		    call pargi (ap[1])
		case 3:
		    switch (SMW_LAXIS(smw,1)) {
		    case 1:
			call sprintf (section, sz_section, "[*,%d,%d]")
		    case 2:
			call sprintf (section, sz_section, "[%d,*,%d]")
		    case 3:
			call sprintf (section, sz_section, "[%d,%d,*]")
		    }
		    #call pargi (LINDEX(sh,1))
		    #call pargi (LINDEX(sh,2))
		    call pargi (ap[1])
		    call pargi (ap[2])
		}
	    }
	case SMW_ES, SMW_MS:
	    call sprintf (section, sz_section, " - Ap %d")
		call pargi (ap[1])
	}
end
