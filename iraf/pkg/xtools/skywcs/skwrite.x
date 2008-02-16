include "skywcsdef.h"
include "skywcs.h"


# SK_IIPRINT -- Print a summary of the input image or list coordinate system.

procedure sk_iiprint (label, imagesys, mw, coo)

char	label[ARB]		#I the input label
char	imagesys[ARB]		#I the input image name and wcs
pointer	mw			#I pointer to the image wcs
pointer	coo			#I pointer to the coordinate system structure

begin
	if (mw == NULL)
	    call sk_inprint (label, imagesys, SKY_CTYPE(coo),
	        SKY_RADECSYS(coo), SKY_EQUINOX(coo), SKY_EPOCH(coo))
	else
	    call sk_imprint (label, imagesys, SKY_CTYPE(coo), SKY_PLNGAX(coo),
	        SKY_PLATAX(coo), SKY_WTYPE(coo), SKY_PIXTYPE(coo),
		SKY_RADECSYS(coo), SKY_EQUINOX(coo), SKY_EPOCH(coo))
end


# SK_IIWRITE -- Write a summary of the input image or list coordinate system
# to the output file

procedure sk_iiwrite (fd, label, imagesys, mw, coo)

int	fd			#I the output file descriptor
char	label[ARB]		#I the input label
char	imagesys[ARB]		#I the input image name and wcs
pointer	mw			#I pointer to the image wcs
pointer	coo			#I pointer to the coordinate system structure

begin
	if (mw == NULL)
	    call sk_inwrite (fd, label, imagesys, SKY_CTYPE(coo),
	        SKY_RADECSYS(coo), SKY_EQUINOX(coo), SKY_EPOCH(coo))
	else
	    call sk_imwrite (fd, label, imagesys, SKY_CTYPE(coo),
	        SKY_PLNGAX(coo), SKY_PLATAX(coo), SKY_WTYPE(coo),
		SKY_PIXTYPE(coo), SKY_RADECSYS(coo), SKY_EQUINOX(coo),
		SKY_EPOCH(coo))
end


# SK_INPRINT -- Print a summary of the input list coordinate system.
# This should probably be a call to sk_inwrite with the file descriptor
# set to STDOUT to avoid duplication of code. There was a reason for
# having two routines at one point but I can't remember what it was ...

procedure sk_inprint (label, system, ctype, radecsys, equinox, epoch)

char	label[ARB]		#I the input label
char	system[ARB]		#I the input system
int	ctype			#I the input coordinate type
int	radecsys		#I the input equatorial reference system
double	equinox			#I the input equinox
double	epoch			#I the input epoch of the observation

pointer	sp, radecstr
double	sl_epj(), sl_epb()
int	sk_wrdstr()

begin
	call smark (sp)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)

	switch (ctype) {

	case CTYPE_EQUATORIAL:
	    if (sk_wrdstr (radecsys, Memc[radecstr], SZ_FNAME,
	        EQTYPE_LIST) <= 0)
	        call strcpy ("FK5", Memc[radecstr], SZ_FNAME)
	    call strupr (Memc[radecstr])
	    call printf ("%s: %s  Coordinates: equatorial %s\n")
	        call pargstr (label)
		call pargstr (system)
		call pargstr (Memc[radecstr])
	    switch (radecsys) {
	    case EQTYPE_GAPPT:
		call printf ("    MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		    call pargd (epoch)
		    if (IS_INDEFD(epoch)) {
			call pargd (INDEFD)
			call pargd (INDEFD)
		    } else {
		        call pargd (sl_epj (epoch))
		        call pargd (sl_epb (epoch))
		    }
	    case EQTYPE_FK5, EQTYPE_ICRS:
		call printf ("    Equinox: J%0.3f Epoch: J%0.8f MJD: %0.5f\n")
		    call pargd (equinox)
		    call pargd (sl_epj(epoch))
		    call pargd (epoch)
	    default:
		call printf ("    Equinox: B%0.3f Epoch: B%0.8f MJD: %0.5f\n")
		    call pargd (equinox)
		    call pargd (sl_epb(epoch))
		    call pargd (epoch)
	    }

	case CTYPE_ECLIPTIC:
	    call printf ("%s: %s  Coordinates: ecliptic\n")
		call pargstr (label)
		call pargstr (system)
	    call printf ("    MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		if (IS_INDEFD(epoch)) {
		    call pargd (INDEFD)
		    call pargd (INDEFD)
		} else {
		    call pargd (sl_epj(epoch))
		    call pargd (sl_epb(epoch))
		}

	case CTYPE_GALACTIC:
	    call printf ("%s: %s  Coordinates: galactic\n")
		call pargstr (label)
		call pargstr (system)
	    call printf ("    MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		call pargd (sl_epj (epoch))
		call pargd (sl_epb (epoch))

	case CTYPE_SUPERGALACTIC:
	    call printf ("%s: %s  Coordinates: supergalactic\n")
		call pargstr (label)
		call pargstr (system)
	    call printf ("    MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		call pargd (sl_epj (epoch))
		call pargd (sl_epb (epoch))

	}

	call sfree (sp)
end


# SK_INWRITE -- Write a summary of the input coordinate system.

procedure sk_inwrite (fd, label, system, ctype, radecsys, equinox, epoch)

int	fd			#I the output file descriptor
char	label[ARB]		#I the input label
char	system[ARB]		#I the input system
int	ctype			#I the input coordinate type
int	radecsys		#I the input equatorial reference system
double	equinox			#I the input equinox
double	epoch			#I the input epoch of the observation

pointer	sp, radecstr
double	sl_epj(), sl_epb()
int	sk_wrdstr()

begin
	call smark (sp)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)

	switch (ctype) {

	case CTYPE_EQUATORIAL:
	    if (sk_wrdstr (radecsys, Memc[radecstr], SZ_FNAME,
	        EQTYPE_LIST) <= 0)
	        call strcpy ("FK5", Memc[radecstr], SZ_FNAME)
	    call strupr (Memc[radecstr])
	    call fprintf (fd, "# %s: %s  Coordinates: equatorial %s\n")
	        call pargstr (label)
	        call pargstr (system)
		call pargstr (Memc[radecstr])
	    switch (radecsys) {
	    case EQTYPE_GAPPT:
		call fprintf (fd, "#     MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		    call pargd (epoch)
		    if (IS_INDEFD(epoch)) {
		        call pargd (INDEFD)
		        call pargd (INDEFD)
		    } else {
		        call pargd (sl_epj(epoch))
		        call pargd (sl_epb(epoch))
		    }
	    case EQTYPE_FK5, EQTYPE_ICRS:
		call fprintf (fd,
		    "#     Equinox: J%0.3f Epoch: J%0.8f MJD: %0.5f\n")
		    call pargd (equinox)
		    call pargd (sl_epj(epoch))
		    call pargd (epoch)
	    default:
		call fprintf (fd,
		    "#     Equinox: B%0.3f Epoch: B%0.8f MJD: %0.5f\n")
		    call pargd (equinox)
		    call pargd (sl_epb(epoch))
		    call pargd (epoch)
	    }

	case CTYPE_ECLIPTIC:
	    call fprintf (fd, "# %s: %s  Coordinates: ecliptic\n")
		call pargstr (label)
		call pargstr (system)
	    call fprintf (fd, "#     MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		if (IS_INDEFD(epoch)) {
		    call pargd (INDEFD)
		    call pargd (INDEFD)
		} else {
		    call pargd (sl_epj(epoch))
		    call pargd (sl_epb(epoch))
		}

	case CTYPE_GALACTIC:
	    call fprintf (fd, "# %s: %s  Coordinates: galactic\n")
		call pargstr (label)
		call pargstr (system)
	    call fprintf (fd, "#     MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		call pargd (sl_epj(epoch))
		call pargd (sl_epb(epoch))

	case CTYPE_SUPERGALACTIC:
	    call fprintf (fd, "# %s: %s  Coordinates: supergalactic\n")
		call pargstr (label)
		call pargstr (system)
	    call fprintf (fd, "#     MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		call pargd (sl_epj(epoch))
		call pargd (sl_epb(epoch))

	}

	call sfree (sp)
end


# SK_IMPRINT -- Print a summary of the input image coordinate system.
# This should probably be a call to sk_imwrite with the file descriptor
# set to STDOUT to avoid duplication of code. There was a reason for
# having two routines at one point but I can't remember what it was ...

procedure sk_imprint (label, imagesys, ctype, lngax, latax, wtype, ptype,
	radecsys, equinox, epoch)

char	label[ARB]		#I input label
char	imagesys[ARB]		#I the input image name and system
int	ctype			#I the image coordinate type
int	lngax			#I the image ra/glon/elon axis
int	latax			#I the image dec/glat/elat axis
int	wtype			#I the image projection type
int	ptype			#I the image image wcs type
int	radecsys		#I the image equatorial reference system
double	equinox			#I the image equinox
double	epoch			#I the image epoch of the observation

pointer	sp, imname, projstr, wcsstr, radecstr
double	sl_epj(), sl_epb()
int	sk_wrdstr()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (projstr, SZ_FNAME, TY_CHAR)
	call salloc (wcsstr, SZ_FNAME, TY_CHAR)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)

	call sscan (imagesys)
	    call gargwrd (Memc[imname], SZ_FNAME)
	if (sk_wrdstr (wtype, Memc[projstr], SZ_FNAME, WTYPE_LIST) <= 0)
	    call strcpy ("linear", Memc[projstr], SZ_FNAME)
	call strupr (Memc[projstr])
	if (sk_wrdstr (ptype, Memc[wcsstr], SZ_FNAME, PIXTYPE_LIST) <= 0)
	    call strcpy ("world", Memc[wcsstr], SZ_FNAME)
	call strlwr (Memc[wcsstr])

	switch (ctype) {

	case CTYPE_EQUATORIAL:
	    if (sk_wrdstr (radecsys, Memc[radecstr], SZ_FNAME,
	        EQTYPE_LIST) <= 0)
	        call strcpy ("FK5", Memc[radecstr], SZ_FNAME)
	    call strupr (Memc[radecstr])
	    call printf (
	    "%s: %s %s  Projection: %s  Ra/Dec axes: %d/%d\n")
		call pargstr (label)
		call pargstr (Memc[imname])
		call pargstr (Memc[wcsstr])
	        call pargstr (Memc[projstr])
	        call pargi (lngax)
	        call pargi (latax)
	    switch (radecsys) {
	    case EQTYPE_GAPPT:
	        call printf ("    Coordinates: equatorial %s\n")
		    call pargstr (Memc[radecstr])
		call printf ("    MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		    call pargd (epoch)
		    if (IS_INDEFD(epoch)) {
			call pargd (INDEFD)
			call pargd (INDEFD)
		    } else {
		        call pargd (sl_epj(epoch))
		        call pargd (sl_epb(epoch))
		    }
	    case EQTYPE_FK5, EQTYPE_ICRS:
	        call printf ("    Coordinates: equatorial %s Equinox: J%0.3f\n")
		    call pargstr (Memc[radecstr])
		    call pargd (equinox)
	        call printf ("    Epoch: J%0.8f MJD: %0.5f\n")
		    call pargd (sl_epj (epoch))
		    call pargd (epoch)
	    default:
	        call printf ("    Coordinates: equatorial %s Equinox: B%0.3f\n")
		    call pargstr (Memc[radecstr])
		    call pargd (equinox)
	        call printf ("    Epoch: B%0.8f MJD: %0.5f\n")
		    call pargd (sl_epb (epoch))
		    call pargd (epoch)
	    }

	case CTYPE_ECLIPTIC:
	    call printf (
	    "%s: %s %s  Projection: %s  Elong/Elat axes: %d/%d\n")
		call pargstr (label)
		call pargstr (Memc[imname])
		call pargstr (Memc[wcsstr])
	        call pargstr (Memc[projstr])
	        call pargi (lngax)
	        call pargi (latax)
	    call printf ("    Coordinates: ecliptic\n")
	    call printf ("    MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		if (IS_INDEFD(epoch)) {
		    call pargd (INDEFD)
		    call pargd (INDEFD)
		} else {
		    call pargd (sl_epj(epoch))
		    call pargd (sl_epb(epoch))
		}

	case CTYPE_GALACTIC:
	    call printf (
	    "%s: %s %s  Projection: %s  Glong/Glat axes: %d/%d\n")
		call pargstr (label)
		call pargstr (Memc[imname])
		call pargstr (Memc[wcsstr])
	        call pargstr (Memc[projstr])
	        call pargi (lngax)
	        call pargi (latax)
	    call printf ("    Coordinates: galactic\n")
	    call printf ("    MJD: %0.5f  Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		call pargd (sl_epj (epoch))
		call pargd (sl_epb (epoch))

	case CTYPE_SUPERGALACTIC:
	    call printf (
	    "%s: %s %s  Projection: %s  Slong/Slat axes: %d/%d\n")
		call pargstr (label)
		call pargstr (Memc[imname])
		call pargstr (Memc[wcsstr])
	        call pargstr (Memc[projstr])
	        call pargi (lngax)
	        call pargi (latax)
	    call printf ("    Coordinates: supergalactic\n")
	    call printf ("    MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		call pargd (sl_epj (epoch))
		call pargd (sl_epb (epoch))
	}

	call sfree (sp)
end


# SK_IMWRITE -- Write a summary of the image coordinate system to the
# output file.

procedure sk_imwrite (fd, label, imagesys, ctype, lngax, latax, wtype, ptype,
	radecsys, equinox, epoch)

int	fd			#I the output file descriptor
char	label[ARB]		#I input label
char	imagesys[ARB]		#I the input image name and wcs
int	ctype			#I the image coordinate type
int	lngax			#I the image ra/glon/elon axis
int	latax			#I the image dec/glat/elat axis
int	wtype			#I the image projection type
int	ptype			#I the image image wcs type
int	radecsys		#I the image equatorial reference system
double	equinox			#I the image equinox
double	epoch			#I the image epoch of the observation

pointer	sp, imname, projstr, wcsstr, radecstr
double	sl_epj(), sl_epb()
int	sk_wrdstr()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (projstr, SZ_FNAME, TY_CHAR)
	call salloc (wcsstr, SZ_FNAME, TY_CHAR)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)

	call sscan (imagesys)
	    call gargwrd (Memc[imname], SZ_FNAME)
	if (sk_wrdstr (wtype, Memc[projstr], SZ_FNAME, WTYPE_LIST) <= 0)
	    call strcpy ("linear", Memc[projstr], SZ_FNAME)
	call strupr (Memc[projstr])
	if (sk_wrdstr (ptype, Memc[wcsstr], SZ_FNAME, PIXTYPE_LIST) <= 0)
	    call strcpy ("world", Memc[wcsstr], SZ_FNAME)
	call strlwr (Memc[wcsstr])

	switch (ctype) {

	case CTYPE_EQUATORIAL:
	    if (sk_wrdstr (radecsys, Memc[radecstr], SZ_FNAME,
	        EQTYPE_LIST) <= 0)
	        call strcpy ("FK5", Memc[radecstr], SZ_FNAME)
	    call strupr (Memc[radecstr])
	    call fprintf (fd,
	    "# %s: %s %s  Projection: %s  Ra/Dec axes: %d/%d\n")
		call pargstr (label)
		call pargstr (Memc[imname])
		call pargstr (Memc[wcsstr])
	        call pargstr (Memc[projstr])
	        call pargi (lngax)
	        call pargi (latax)
	    switch (radecsys) {
	    case EQTYPE_GAPPT:
	        call fprintf (fd, "#     Coordinates: equatorial %s\n")
		    call pargstr (Memc[radecstr])
		call fprintf (fd, "#     MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		    call pargd (epoch)
		    if (IS_INDEFD(epoch)) {
		        call pargd (INDEFD)
		        call pargd (INDEFD)
		    } else {
		        call pargd (sl_epj(epoch))
		        call pargd (sl_epb(epoch))
		    }
	    case EQTYPE_FK5, EQTYPE_ICRS:
	        call fprintf (fd,
		    "#     Coordinates: equatorial %s Equinox: J%0.3f\n")
		    call pargstr (Memc[radecstr])
		    call pargd (equinox)
	        call fprintf (fd, "#     Epoch: J%0.8f MJD: %0.5f\n")
		    call pargd (sl_epj(epoch))
		    call pargd (epoch)
	    default:
	        call fprintf (fd,
		    "#     Coordinates: equatorial %s Equinox: B%0.3f\n")
		    call pargstr (Memc[radecstr])
		    call pargd (equinox)
	        call fprintf (fd, "#     Epoch: B%0.8f MJD: %0.5f\n")
		    call pargd (sl_epb (epoch))
		    call pargd (epoch)
	    }

	case CTYPE_ECLIPTIC:
	    call fprintf (fd,
	    "# %s: %s %s  Projection: %s  Elong/Elat axes: %d/%d\n")
		call pargstr (label)
		call pargstr (Memc[imname])
		call pargstr (Memc[wcsstr])
	        call pargstr (Memc[projstr])
	        call pargi (lngax)
	        call pargi (latax)
	    call fprintf (fd, "#     Coordinates: ecliptic\n")
	    call fprintf (fd, "#     MJD: %0.5f  Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		if (IS_INDEFD(epoch)) {
		    call pargd (INDEFD)
		    call pargd (INDEFD)
		} else {
		    call pargd (sl_epj(epoch))
		    call pargd (sl_epb(epoch))
		}

	case CTYPE_GALACTIC:
	    call fprintf (fd,
	    "# %s: %s %s  Projection: %s  Glong/Glat axes: %d/%d\n")
		call pargstr (label)
		call pargstr (Memc[imname])
		call pargstr (Memc[wcsstr])
	        call pargstr (Memc[projstr])
	        call pargi (lngax)
	        call pargi (latax)
	    call fprintf (fd, "#     Coordinates: galactic\n")
	    call fprintf (fd, "#     MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		call pargd (sl_epj(epoch))
		call pargd (sl_epb(epoch))

	case CTYPE_SUPERGALACTIC:
	    call fprintf (fd,
	    "# %s: %s %s  Projection: %s  Slong/Slat axes: %d/%d\n")
		call pargstr (label)
		call pargstr (Memc[imname])
		call pargstr (Memc[wcsstr])
	        call pargstr (Memc[projstr])
	        call pargi (lngax)
	        call pargi (latax)
	    call fprintf (fd, "#     Coordinates: supergalactic\n")
	    call fprintf (fd, "#     MJD: %0.5f Epoch: J%0.8f B%0.8f\n")
		call pargd (epoch)
		call pargd (sl_epj(epoch))
		call pargd (sl_epb(epoch))
	}

	call sfree (sp)
end
