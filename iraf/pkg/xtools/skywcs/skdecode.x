include <imio.h>
include <imhdr.h>
include <mwset.h>
include "skywcs.h"
include "skywcsdef.h"

# SK_DECWCS -- Decode the wcs string which may be either an image name
# plus wcs, e.g. "dev$pix logical" or a string describing the celestial
# coordinate system, e.g. "J2000" or "galactic" into a celestial coordinate
# structure. If the input wcs is an image wcs then a non-NULL pointer to
# the image wcs structure is also returned. ERR is returned if a valid
# celestial coordinate structure cannot be created.

int procedure sk_decwcs (instr, mw, coo, imcoo)

char	instr[ARB]		#I the input wcs string
pointer	mw			#O the pointer to the image wcs structure
pointer	coo			#O the pointer to the coordinate structure
pointer	imcoo			#I pointer to an existing coordinate structure 

int	stat
pointer	sp, str1, str2, laxno, paxval, im
int	sk_strwcs(), sk_decim()
pointer	immap()
errchk	immap()

begin
	call calloc (coo, LEN_SKYCOOSTRUCT, TY_STRUCT)
	call strcpy (instr, SKY_COOSYSTEM(coo), SZ_FNAME)

	# Allocate some working space.
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	call salloc (laxno, IM_MAXDIM, TY_INT)
	call salloc (paxval, IM_MAXDIM, TY_INT)

	# Decode the wcs.
	call sscan (instr)
	    call gargwrd (Memc[str1], SZ_LINE)
	    call gargwrd (Memc[str2], SZ_LINE)

	# First try to open an image wcs.
	iferr {
	    im = immap (Memc[str1], READ_ONLY, 0)

	# Decode the user wcs.
	} then {

	    # Initialize.
	    mw = NULL
	    if (imcoo == NULL) {
	        SKY_NLNGAX(coo) = 2048
	        SKY_NLATAX(coo) = 2048
	        SKY_PLNGAX(coo) = 1
	        SKY_PLATAX(coo) = 2
	        SKY_XLAX(coo) = 1
	        SKY_YLAX(coo) = 2
	        SKY_VXOFF(coo) = 0.0d0
	        SKY_VYOFF(coo) = 0.0d0
	        SKY_VXSTEP(coo) = 1.0d0
	        SKY_VYSTEP(coo) = 1.0d0
	        SKY_WTYPE(coo) = 0
	    } else {
	        SKY_NLNGAX(coo) = SKY_NLNGAX(imcoo)
	        SKY_NLATAX(coo) = SKY_NLATAX(imcoo)
	        SKY_PLNGAX(coo) = SKY_PLNGAX(imcoo)
	        SKY_PLATAX(coo) = SKY_PLATAX(imcoo)
	        SKY_XLAX(coo) = SKY_XLAX(imcoo)
	        SKY_YLAX(coo) = SKY_YLAX(imcoo)
	        SKY_VXOFF(coo) = SKY_VXOFF(imcoo)
	        SKY_VYOFF(coo) = SKY_VYOFF(imcoo)
	        SKY_VXSTEP(coo) = SKY_VXSTEP(imcoo)
	        SKY_VYSTEP(coo) = SKY_VYSTEP(imcoo)
	        SKY_WTYPE(coo) = SKY_WTYPE(imcoo)
	    }
	    SKY_PIXTYPE(coo) = PIXTYPE_WORLD

	    # Decode the actual wcs.
	    stat = sk_strwcs (instr, SKY_CTYPE(coo), SKY_RADECSYS(coo),
	        SKY_EQUINOX(coo), SKY_EPOCH(coo))
	    switch (SKY_CTYPE(coo)) {
	    case CTYPE_EQUATORIAL:
		SKY_NLNGUNITS(coo) = SKY_HOURS
		SKY_NLATUNITS(coo) = SKY_DEGREES
	    default:
		SKY_NLNGUNITS(coo) = SKY_DEGREES
		SKY_NLATUNITS(coo) = SKY_DEGREES
	    }

	# Decode the image wcs.
	} else {
	    stat = sk_decim (im, Memc[str2], mw, coo)
	    call imunmap (im)
	}

	call sfree (sp)

	SKY_STATUS(coo) = stat
	return (stat)
end


# SK_DECWSTR -- Decode the wcs string coordinate system, e.g. "J2000" or
# "galactic" into a celestial coordinate structure. ERR is returned if a
# valid celestial coordinate structure cannot be created.

int procedure sk_decwstr (instr, coo, imcoo)

char	instr[ARB]		#I the input wcs string
pointer	coo			#O the pointer to the coordinate structure
pointer	imcoo			#I pointer to an existing coordinate structure 

int	stat
int	sk_strwcs()

begin
	call calloc (coo, LEN_SKYCOOSTRUCT, TY_STRUCT)
	call strcpy (instr, SKY_COOSYSTEM(coo), SZ_FNAME)

	# Initialize.
	if (imcoo == NULL) {
	    SKY_NLNGAX(coo) = 2048
	    SKY_NLATAX(coo) = 2048
	    SKY_PLNGAX(coo) = 1
	    SKY_PLATAX(coo) = 2
	    SKY_XLAX(coo) = 1
	    SKY_YLAX(coo) = 2
	    SKY_VXOFF(coo) = 0.0d0
	    SKY_VYOFF(coo) = 0.0d0
	    SKY_VXSTEP(coo) = 1.0d0
	    SKY_VYSTEP(coo) = 1.0d0
	    SKY_WTYPE(coo) = 0
	} else {
	    SKY_NLNGAX(coo) = SKY_NLNGAX(imcoo)
	    SKY_NLATAX(coo) = SKY_NLATAX(imcoo)
	    SKY_PLNGAX(coo) = SKY_PLNGAX(imcoo)
	    SKY_PLATAX(coo) = SKY_PLATAX(imcoo)
	    SKY_XLAX(coo) = SKY_XLAX(imcoo)
	    SKY_YLAX(coo) = SKY_YLAX(imcoo)
	    SKY_VXOFF(coo) = SKY_VXOFF(imcoo)
	    SKY_VYOFF(coo) = SKY_VYOFF(imcoo)
	    SKY_VXSTEP(coo) = SKY_VXSTEP(imcoo)
	    SKY_VYSTEP(coo) = SKY_VYSTEP(imcoo)
	    SKY_WTYPE(coo) = SKY_WTYPE(imcoo)
	}
	SKY_PIXTYPE(coo) = PIXTYPE_WORLD

	# Decode the actual wcs.
	stat = sk_strwcs (instr, SKY_CTYPE(coo), SKY_RADECSYS(coo),
            SKY_EQUINOX(coo), SKY_EPOCH(coo))
	switch (SKY_CTYPE(coo)) {
	case CTYPE_EQUATORIAL:
	    SKY_NLNGUNITS(coo) = SKY_HOURS
	    SKY_NLATUNITS(coo) = SKY_DEGREES
	default:
	    SKY_NLNGUNITS(coo) = SKY_DEGREES
	    SKY_NLATUNITS(coo) = SKY_DEGREES
	}

	SKY_STATUS(coo) = stat

	return (stat)
end


# SK_DECIM -- Given an image descriptor and an image wcs string create a
# celstial coordinate structure. A non-NULL pointer to the image wcs structure
# is also returned. ERR is returned if a valid  celestial coordinate descriptor
# cannot be created.


int procedure sk_decim (im, wcs, mw, coo)

pointer	im			#I the pointer to the input image
char	wcs[ARB]		#I the wcs string [logical|tv|physical|world]
pointer	mw			#O the pointer to the image wcs structure
pointer	coo			#O the pointer to the coordinate structure

int	stat
pointer	sp, str1, laxno, paxval
int	sk_imwcs(), strdic(), mw_stati()
pointer	mw_openim()
errchk	mw_openim()

begin
	call malloc (coo, LEN_SKYCOOSTRUCT, TY_STRUCT)
	call sprintf (SKY_COOSYSTEM(coo), SZ_FNAME, "%s %s")
	    call pargstr (IM_HDRFILE(im))
	    call pargstr (wcs)

	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (laxno, IM_MAXDIM, TY_INT)
	call salloc (paxval, IM_MAXDIM, TY_INT)

	# Try to open the image wcs.
	iferr {
	    mw = mw_openim (im)

	# Set up a dummy wcs.
	} then {

	    #Initialize.
	    SKY_CTYPE(coo) = 0
	    SKY_RADECSYS(coo) = 0
	    SKY_EQUINOX(coo) = INDEFD
	    SKY_EPOCH(coo) = INDEFD
	    mw = NULL
	    SKY_PLNGAX(coo) = 1
	    SKY_PLATAX(coo) = 2
	    SKY_XLAX(coo) = 1
	    SKY_YLAX(coo) = 2
	    SKY_NLNGAX(coo) = 2048
	    SKY_NLATAX(coo) = 2048
	    SKY_VXOFF(coo) = 0.0d0
	    SKY_VYOFF(coo) = 0.0d0
	    SKY_VXSTEP(coo) = 1.0d0
	    SKY_VYSTEP(coo) = 1.0d0
	    SKY_WTYPE(coo) = 0
	    SKY_PIXTYPE(coo) = PIXTYPE_LOGICAL
	    SKY_NLNGUNITS(coo) = SKY_DEGREES
	    SKY_NLATUNITS(coo) = SKY_DEGREES
	    stat = ERR

	# Decode the wcs.
	} else {
	    SKY_PIXTYPE(coo) = strdic (wcs, Memc[str1], SZ_LINE, PIXTYPE_LIST)
	    if (SKY_PIXTYPE(coo) <= 0)
	        SKY_PIXTYPE(coo) = PIXTYPE_LOGICAL
	    if (sk_imwcs (im, mw, SKY_CTYPE(coo), SKY_PLNGAX(coo),
		    SKY_PLATAX(coo), SKY_WTYPE(coo), SKY_RADECSYS(coo),
		    SKY_EQUINOX(coo), SKY_EPOCH(coo)) == OK) {
	    	switch (SKY_CTYPE(coo)) {
	    	case CTYPE_EQUATORIAL:
	    	    SKY_NLNGUNITS(coo) = SKY_HOURS
	    	    SKY_NLATUNITS(coo) = SKY_DEGREES
	    	default:
	    	    SKY_NLNGUNITS(coo) = SKY_DEGREES
	    	    SKY_NLATUNITS(coo) = SKY_DEGREES
	    	}
		call mw_gaxmap (mw, Memi[laxno], Memi[paxval], mw_stati(mw,
		    MW_NPHYSDIM))
		if (Memi[laxno+SKY_PLNGAX(coo)-1] <
		    Memi[laxno+SKY_PLATAX(coo)-1]) {
		    SKY_XLAX(coo) = Memi[laxno+SKY_PLNGAX(coo)-1] 
		    SKY_YLAX(coo) = Memi[laxno+SKY_PLATAX(coo)-1] 
		} else {
		    SKY_XLAX(coo) = Memi[laxno+SKY_PLATAX(coo)-1] 
		    SKY_YLAX(coo) = Memi[laxno+SKY_PLNGAX(coo)-1] 
		}
		if (SKY_XLAX(coo) <= 0 || SKY_YLAX(coo) <= 0) {
		    SKY_VXOFF(coo) = 0.0d0
		    SKY_VYOFF(coo) = 0.0d0
		    SKY_VXSTEP(coo) = 1.0d0
		    SKY_VYSTEP(coo) = 1.0d0
	            SKY_NLNGAX(coo) = 2048
	            SKY_NLATAX(coo) = 2048
		    stat = ERR
		} else {
		    SKY_VXOFF(coo) = IM_VOFF(im,IM_VMAP(im,SKY_XLAX(coo)))
		    SKY_VYOFF(coo) = IM_VOFF(im,IM_VMAP(im,SKY_YLAX(coo)))
		    SKY_VXSTEP(coo) = IM_VSTEP(im,SKY_XLAX(coo))
		    SKY_VYSTEP(coo) = IM_VSTEP(im,SKY_YLAX(coo))
	            SKY_NLNGAX(coo) = IM_LEN(im,SKY_XLAX(coo))
	            SKY_NLATAX(coo) = IM_LEN(im,SKY_YLAX(coo))
		    stat = OK
		}
	    } else {
		call mw_close (mw)
		mw = NULL
	        SKY_XLAX(coo) = 1
	        SKY_YLAX(coo) = 2
	        SKY_NLNGAX(coo) = 2048
	        SKY_NLATAX(coo) = 2048
	        SKY_VXOFF(coo) = 0.0d0
	        SKY_VYOFF(coo) = 0.0d0
	        SKY_VXSTEP(coo) = 1.0d0
	        SKY_VYSTEP(coo) = 1.0d0
	    	SKY_NLNGUNITS(coo) = SKY_DEGREES
	    	SKY_NLATUNITS(coo) = SKY_DEGREES
		stat = ERR
	    }
	}

	call sfree (sp)

	SKY_STATUS(coo) = stat
	return (stat)
end


# SK_STRWCS -- Decode the sky coordinate system from an input string.
# The string syntax is [ctype] equinox [epoch]. The various options
# have been placed case statements. Although there is considerable
# duplication of code in the case statements, there are minor differences
# and I found it clearer to write it out rather than trying to be
# concise. I might want to clean this up a bit later.

int procedure sk_strwcs (instr, ctype, radecsys, equinox, epoch)

char	instr[ARB]		#I the input wcs string
int	ctype			#O the output coordinate type
int	radecsys		#O the output equatorial reference system
double	equinox			#O the output equinox
double	epoch			#O the output epoch of the observation

int	ip, nitems, sctype, sradecsys, stat
pointer	sp, str1, str2
int	strdic(), nscan(), ctod()
double	sl_ej2d(), sl_epb(), sl_eb2d(), sl_epj()

begin
	# Initialize.
	ctype = 0
	radecsys = 0
	equinox = INDEFD
	epoch = INDEFD

	# Allocate working space.
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Determine the coordinate string.
	call sscan (instr)
	    call gargwrd (Memc[str1], SZ_LINE)

	# Return with an error if the string is blank.
	if (Memc[str1] == EOS || nscan() < 1) {
	    call sfree (sp)
	    return (ERR)
	} else
	    nitems = 1
	    
	# If the coordinate type is undefined temporarily default it to
	# equatorial.
	sctype = strdic (Memc[str1], Memc[str2], SZ_LINE, FTYPE_LIST) 
	if (sctype <= 0) {
	    ctype = CTYPE_EQUATORIAL
	} else {
	    switch (sctype) {
	    case FTYPE_FK4:
	        ctype = CTYPE_EQUATORIAL
		radecsys = EQTYPE_FK4
	    case FTYPE_FK4NOE:
	        ctype = CTYPE_EQUATORIAL
		radecsys = EQTYPE_FK4NOE
	    case FTYPE_FK5:
	        ctype = CTYPE_EQUATORIAL
		radecsys = EQTYPE_FK5
	    case FTYPE_ICRS:
	        ctype = CTYPE_EQUATORIAL
		radecsys = EQTYPE_ICRS
	    case FTYPE_GAPPT:
	        ctype = CTYPE_EQUATORIAL
		radecsys = EQTYPE_GAPPT
	    case FTYPE_ECLIPTIC:
	        ctype = CTYPE_ECLIPTIC
	    case FTYPE_GALACTIC:
	        ctype = CTYPE_GALACTIC
	    case FTYPE_SUPERGALACTIC:
	        ctype = CTYPE_SUPERGALACTIC
	    }
	    call gargwrd (Memc[str1], SZ_LINE)
	    if (nscan() > nitems)
		nitems = nitems + 1
	}
	sctype = ctype
	sradecsys = radecsys

	# Decode the coordinate system.
	switch (sctype) {

	# Decode the equatorial system, equinox, and epoch.
	case CTYPE_EQUATORIAL:

	    switch (sradecsys) {
	    case EQTYPE_FK4, EQTYPE_FK4NOE:
		if (Memc[str1] == 'J' || Memc[str1] == 'j' ||
		    Memc[str1] == 'B' || Memc[str1] == 'b')
		    ip = 2
		else 
		    ip = 1
	        if (ctod (Memc[str1], ip, equinox) <= 0)
		    equinox = 1950.0d0
		if (Memc[str1] == 'J' || Memc[str1] == 'j')
		    equinox = sl_epb (sl_ej2d (equinox))

	        call gargwrd (Memc[str2], SZ_LINE)
	        if (nscan() <= nitems)
		    epoch = sl_eb2d (equinox)
		else {
		    if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		        Memc[str2] == 'B' || Memc[str2] == 'b')
		        ip = 2
		    else
		        ip = 1
		    if (ctod (Memc[str2], ip, epoch) <= 0)
		        epoch = sl_eb2d (equinox)
		    else if (epoch <= 3000.0d0 && (Memc[str2] == 'J' ||
		        Memc[str2] == 'j'))
		        epoch = sl_ej2d (epoch)
		    else if (epoch > 3000.0d0)
		        epoch = epoch - 2400000.5d0
		    else
			epoch = sl_eb2d (epoch)
		}

	    case EQTYPE_FK5, EQTYPE_ICRS:
		if (Memc[str1] == 'J' || Memc[str1] == 'j' ||
		    Memc[str1] == 'B' || Memc[str1] == 'b')
		    ip = 2
		else 
		    ip = 1
	        if (ctod (Memc[str1], ip, equinox) <= 0)
		    equinox = 2000.0d0
		if (Memc[str1] == 'B' || Memc[str1] == 'b')
		    equinox = sl_epj(sl_eb2d (equinox))

	        call gargwrd (Memc[str2], SZ_LINE)
	        if (nscan() <= nitems)
		    epoch = sl_ej2d (equinox)
		else {
		    if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		        Memc[str2] == 'B' || Memc[str2] == 'b')
		        ip = 2
		    else
		        ip = 1
		    if (ctod (Memc[str2], ip, epoch) <= 0)
		        epoch = sl_ej2d (equinox)
		    else if (epoch <= 3000.0d0 && (Memc[str2] == 'B' ||
		        Memc[str2] == 'b'))
		        epoch = sl_eb2d (epoch)
		    else if (epoch > 3000.0d0)
		        epoch = epoch - 2400000.5d0
		    else
			epoch = sl_ej2d (epoch)
		}

	    case EQTYPE_GAPPT:
		equinox = 2000.0d0
		if (Memc[str1] == 'J' || Memc[str1] == 'j' ||
		    Memc[str1] == 'B' || Memc[str1] == 'b')
		    ip = 2
		else
		    ip = 1
	        if (ctod (Memc[str1], ip, epoch) <= 0) {
		    epoch = INDEFD
		} else if (epoch <= 3000.0d0) {
		    if (Memc[str1] == 'B' || Memc[str1] == 'b')
		        epoch = sl_eb2d (epoch)
		    else if (Memc[str1] == 'J' || Memc[str1] == 'j')
		        epoch = sl_ej2d (epoch)
		    else if (epoch < 1984.0d0)
		        epoch = sl_eb2d (epoch)
		    else
		        epoch = sl_ej2d (epoch)
		} else {
		    epoch = epoch - 2400000.5d0
		} 

	    default:
		ip = 1
		if (Memc[str1] == 'B' || Memc[str1] == 'b') {
		    radecsys = EQTYPE_FK4
		    ip = ip + 1
	            if (ctod (Memc[str1], ip, equinox) <= 0)
			equinox = 1950.0d0

	            call gargwrd (Memc[str2], SZ_LINE)
	            if (nscan() <= nitems)
		        epoch = sl_eb2d (equinox)
		    else {
		        if (Memc[str2] == 'J' || Memc[str2] == 'j')
		            ip = 2
		        else if (Memc[str2] == 'B' || Memc[str2] == 'b')
		            ip = 2
		        else
		            ip = 1
		        if (ctod (Memc[str2], ip, epoch) <= 0)
		            epoch = sl_eb2d (equinox)
		        else if (epoch <= 3000.0d0 && (Memc[str2] == 'J' ||
			    Memc[str2] == 'j'))
		            epoch = sl_ej2d (epoch)
		        else if (epoch > 3000.0d0)
		            epoch = epoch - 2400000.5d0
			else
			    epoch = sl_eb2d (epoch)
		    }

		} else if (Memc[str1] == 'J' || Memc[str1] == 'j') {
		    radecsys = EQTYPE_FK5
		    ip = ip + 1
	            if (ctod (Memc[str1], ip, equinox) <= 0)
			equinox = 2000.0d0

	            call gargwrd (Memc[str2], SZ_LINE)
	            if (nscan() <= nitems)
		        epoch = sl_ej2d (equinox)
		    else {
		        if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		            Memc[str2] == 'B' || Memc[str2] == 'b')
		            ip = 2
		        else
		            ip = 1
		        if (ctod (Memc[str2], ip, epoch) <= 0)
		            epoch = sl_ej2d (equinox)
		        else if (epoch <= 3000.0d0 && (Memc[str2] == 'B' ||
			    Memc[str2] == 'b'))
		            epoch = sl_eb2d (epoch)
		        else if (epoch > 3000.0d0)
		            epoch = epoch - 2400000.5d0
			else
			    epoch = sl_ej2d (epoch)
		    }

		} else if (ctod (Memc[str1], ip, equinox) <= 0) {
		    ctype = 0
		    radecsys = 0
		    equinox = INDEFD
		    epoch = INDEFD

		} else if (equinox < 1984.0d0) {
		    radecsys = EQTYPE_FK4
	            call gargwrd (Memc[str2], SZ_LINE)
	            if (nscan() <= nitems)
		        epoch = sl_eb2d (equinox)
		    else {
		        if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		            Memc[str2] == 'B' || Memc[str2] == 'b')
		            ip = 2
		        else
		            ip = 1
		        if (ctod (Memc[str2], ip, epoch) <= 0)
		            epoch = sl_eb2d (equinox)
		        else if (epoch <= 3000.0d0 && (Memc[str2] == 'J' ||
			    Memc[str2] == 'j'))
		            epoch = sl_ej2d (epoch)
		        else if (epoch > 3000.0d0)
		            epoch = epoch - 2400000.5d0
			else
			    epoch = sl_eb2d (epoch)
		    }

		} else {
		    radecsys = EQTYPE_FK5
	            call gargwrd (Memc[str2], SZ_LINE)
	            if (nscan() <= nitems)
		        epoch = sl_ej2d (equinox)
		    else {
		        if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		            Memc[str2] == 'B' || Memc[str2] == 'b')
		            ip = 2
		        else
		            ip = 1
		        if (ctod (Memc[str2], ip, epoch) <= 0)
		            epoch = sl_ej2d (equinox)
		        else if (epoch <= 3000.0d0 && (Memc[str2] == 'B' ||
			    Memc[str2] == 'b'))
		            epoch = sl_eb2d (epoch)
		        else if (epoch > 3000.0d0)
		            epoch = epoch - 2400000.5d0
			else
			    epoch = sl_ej2d (epoch)
		    }
		}
	    }

	# Decode the ecliptic coordinate system.
	case CTYPE_ECLIPTIC:
	    if (Memc[str1] == 'J' || Memc[str1] == 'j' ||
	        Memc[str1] == 'B' || Memc[str1] == 'b')
		ip = 2
	    else
		ip = 1
	    if (ctod (Memc[str1], ip, epoch) <= 0) {
		epoch = INDEFD
	    } else if (epoch <= 3000.0d0) {
	        if (Memc[str1] == 'B' || Memc[str1] == 'b')
		    epoch = sl_eb2d (epoch)
	        else if (Memc[str1] == 'J' || Memc[str1] == 'j')
		    epoch = sl_ej2d (epoch)
		else if (epoch < 1984.0d0)
		    epoch = sl_eb2d (epoch)
		else
		    epoch = sl_ej2d (epoch)
	    } else {
		epoch = epoch - 2400000.5d0
	    }

	# Decode the galactic and supergalactic coordinate system.
	case CTYPE_GALACTIC, CTYPE_SUPERGALACTIC:
	    if (Memc[str1] == 'J' || Memc[str1] == 'j' ||
		Memc[str1] == 'B' || Memc[str1] == 'b')
		ip = 2
	    else
		ip = 1
	    if (ctod (Memc[str1], ip, epoch) <= 0) {
		epoch = sl_eb2d (1950.0d0)
	    } else if (epoch <= 3000.0d0) {
	        if (Memc[str1] == 'J' || Memc[str1] == 'j')
		    epoch = sl_ej2d (epoch)
	        else if (Memc[str1] == 'B' || Memc[str1] == 'b')
		    epoch = sl_eb2d (epoch)
	        else if (epoch < 1984.0d0) 
		    epoch = sl_eb2d (epoch)
		else
		    epoch = sl_ej2d (epoch)
	    } else {
		epoch = epoch - 2400000.5d0
	    }
	}

	# Return the appropriate error status.
	if (ctype == 0)
	    stat = ERR
	else if (ctype == CTYPE_EQUATORIAL && (radecsys == 0 ||
	    IS_INDEFD(equinox) || IS_INDEFD(epoch)))
	    stat = ERR
	else if (ctype == CTYPE_ECLIPTIC && IS_INDEFD(epoch))
	    stat = ERR
	else
	    stat = OK

	call sfree (sp)

	return (stat)
end


# SK_IMWCS -- Decode the sky coordinate system of the image. Return
# an error if the sky coordinate system is not one of the supported types
# or required information is missing from the image header.

int procedure sk_imwcs (im, mw, ctype, lngax, latax, wtype, radecsys,
	equinox, epoch)

pointer	im			#I the image pointer
pointer	mw			#I pointer to the world coordinate system
int	ctype			#O the output coordinate type
int	lngax			#O the output ra/glon/elon axis
int	latax			#O the output dec/glat/elat axis
int	wtype			#O the output projection type
int	radecsys		#O the output equatorial reference system
double	equinox			#O the output equinox
double	epoch			#O the output epoch of the observation

int	i, ndim, axtype, day, month, year, ier, oldfits
pointer	sp, atval
double	hours
double	imgetd(), sl_eb2d(), sl_ej2d()
int	mw_stati(), strdic(), dtm_decode()
errchk	mw_gwattrs(), imgstr(), imgetd()

begin
	call smark (sp)
	call salloc (atval, SZ_LINE, TY_CHAR)

	# Initialize
	ctype = 0
	lngax = 0
	latax = 0
	wtype = 0
	radecsys = 0
	equinox = INDEFD
	epoch = INDEFD

	# Determine the sky coordinate system of the image.
	ndim = mw_stati (mw, MW_NPHYSDIM)
	do i = 1, ndim {
	    iferr (call mw_gwattrs (mw, i, "axtype", Memc[atval], SZ_LINE))
		call strcpy ("INDEF", Memc[atval], SZ_LINE)
	    axtype = strdic (Memc[atval], Memc[atval], SZ_LINE, AXTYPE_LIST)
	    switch (axtype) {
	    case AXTYPE_RA, AXTYPE_DEC: 
		ctype = CTYPE_EQUATORIAL
	    case AXTYPE_ELON, AXTYPE_ELAT: 
		ctype = CTYPE_ECLIPTIC
	    case AXTYPE_GLON, AXTYPE_GLAT: 
		ctype = CTYPE_GALACTIC
	    case AXTYPE_SLON, AXTYPE_SLAT: 
		ctype = CTYPE_SUPERGALACTIC
	    default:
		;
	    }
	    switch (axtype) {
	    case AXTYPE_RA, AXTYPE_ELON, AXTYPE_GLON, AXTYPE_SLON:
		lngax = i
	    case AXTYPE_DEC, AXTYPE_ELAT, AXTYPE_GLAT, AXTYPE_SLAT:
		latax = i
	    default:
		;
	    }
	}

	# Return if the sky coordinate system cannot be decoded.
	if (ctype == 0 || lngax == 0 || latax == 0) {
	    call sfree (sp)
	    return (ERR)
	}

	# Decode the sky projection.
	iferr {
	    call mw_gwattrs (mw, lngax, "wtype", Memc[atval], SZ_LINE)
	} then {
	    iferr (call mw_gwattrs(mw, latax, "wtype", Memc[atval], SZ_LINE))
		call strcpy ("linear", Memc[atval], SZ_LINE)
	}
	wtype = strdic (Memc[atval], Memc[atval], SZ_LINE, WTYPE_LIST)

	# Return if the sky projection system is not supported.
	if (wtype == 0) {
	    call sfree (sp)
	    return (ERR)
	}

	# Determine the RA/DEC system and equinox.
	if (ctype == CTYPE_EQUATORIAL) {

	    # Get the equinox of the coordinate system. The EQUINOX keyword
	    # takes precedence over EPOCH.
	    iferr {
	        equinox = imgetd (im, "EQUINOX")
	    } then {
		iferr {
	            equinox = imgetd (im, "EPOCH")
		} then {
		    equinox = INDEFD
		}
	    }

	    # Determine which equatorial system will be used. The default
	    # is FK4 if equinox < 1984.0, FK5 if equinox is >= 1984.
	    iferr {
	        call imgstr (im, "RADECSYS", Memc[atval], SZ_LINE)
	    } then {
	        radecsys = 0
	    } else {
		call strlwr (Memc[atval])
	        radecsys = strdic (Memc[atval], Memc[atval], SZ_LINE,
		    EQTYPE_LIST)
	    }
	    if (radecsys == 0) {
		if (IS_INDEFD(equinox))
		    radecsys = EQTYPE_FK5
		else if (equinox < 1984.0d0)
		    radecsys = EQTYPE_FK4
		else
		    radecsys = EQTYPE_FK5
	    }

	    # Get the MJD of the observation. If there is no MJD in the
	    # header use the DATE_OBS keyword value and transform it to
	    # an MJD.
	    iferr {
	        epoch = imgetd (im, "MJD-WCS")
	    } then {
	        iferr {
	            epoch = imgetd (im, "MJD-OBS")
	        } then {
		    iferr {
	                call imgstr (im, "DATE-OBS", Memc[atval], SZ_LINE)
		    } then {
		        epoch = INDEFD
		    } else if (dtm_decode (Memc[atval], year, month, day,
			hours, oldfits) == OK) {
		        call sl_cadj (year, month, day, epoch, ier)
		        if (ier != 0)
			    epoch = INDEFD
			else if (! IS_INDEFD(hours) && hours >= 0.0d0 &&
			    hours <= 24.0d0)
			    epoch = epoch + hours / 24.0d0
		    } else
		        epoch = INDEFD
	        }
	    }

	    # Set the default equinox and epoch appropriate for each
	    # equatorial system if these are undefined.
	    switch (radecsys) {
	    case EQTYPE_FK4, EQTYPE_FK4NOE:
		if (IS_INDEFD(equinox))
		    equinox = 1950.0d0
		if (IS_INDEFD(epoch))
		    epoch = sl_eb2d (1950.0d0)
	    case EQTYPE_FK5, EQTYPE_ICRS:
		if (IS_INDEFD(equinox))
		    equinox = 2000.0d0
		if (IS_INDEFD(epoch))
		    epoch = sl_ej2d (2000.0d0)
	    case EQTYPE_GAPPT:
		equinox = 2000.0d0
		;
	    }

	    # Return if the epoch is undefined. This can only occur if
	    # the equatorial coordinate system is GAPPT and there is NO
	    # epoch of observation in the image header.
	    if (IS_INDEFD(epoch)) {
		call sfree (sp)
		return (ERR)
	    }
	} 

	# Get the MJD of the observation. If there is no MJD in the
	# header use the DATE_OBS keyword value and transform it to
	# an MJD.
	if (ctype == CTYPE_ECLIPTIC) {

	    iferr {
	        epoch = imgetd (im, "MJD-WCS")
	    } then {
	        iferr {
	            epoch = imgetd (im, "MJD-OBS")
	        } then {
		    iferr {
	                call imgstr (im, "DATE-OBS", Memc[atval], SZ_LINE)
		    } then {
		        epoch = INDEFD
		    } else if (dtm_decode (Memc[atval], year, month, day,
			hours, oldfits) == OK) {
		        call sl_cadj (year, month, day, epoch, ier)
		        if (ier != 0)
			    epoch = INDEFD
			else if (! IS_INDEFD(hours) && hours >= 0.0d0 &&
			    hours <= 24.0d0)
			    epoch = epoch + hours / 24.0d0
		    } else
		        epoch = INDEFD
	        }
	    }

	    # Return if the epoch is undefined.
	    if (IS_INDEFD(epoch)) {
		call sfree (sp)
		return (ERR)
	    }
	}

	if (ctype == CTYPE_GALACTIC || ctype == CTYPE_SUPERGALACTIC) {

	    # Get the MJD of the observation. If there is no MJD in the
	    # header use the DATE_OBS keyword value and transform it to
	    # an MJD.
	    iferr {
	        epoch = imgetd (im, "MJD-WCS")
	    } then {
	        iferr {
	            epoch = imgetd (im, "MJD-OBS")
	        } then {
		    iferr {
	                call imgstr (im, "DATE-OBS", Memc[atval], SZ_LINE)
		    } then {
		        epoch = sl_eb2d (1950.0d0)
		    } else if (dtm_decode (Memc[atval], year, month, day,
		        hours, oldfits) == OK) {
		        call sl_cadj (year, month, day, epoch, ier)
		        if (ier != 0)
			    epoch = sl_eb2d (1950.0d0)
			else {
			    if (! IS_INDEFD(hours) && hours >= 0.0d0 &&
				hours <= 24.0d0)
			        epoch = epoch + hours / 24.0d0
			    #if (epoch < 1984.0d0)
		    	        #epoch = sl_eb2d (epoch)
			    #else
		    	        #epoch = sl_ej2d (epoch)
			}
		    } else
		        epoch = sl_eb2d (1950.0d0)
	        }
	    }
	}

	call sfree (sp)

	return (OK)
end


# SK_ENWCS -- Encode the celestial wcs system.

procedure sk_enwcs (coo, wcsstr, maxch)

pointer	coo			#I the celestial coordinate system descriptor
char	wcsstr[ARB]		#O the output wcs string
int	maxch			#I the size of the output string

double	sk_statd(), sl_epj(), sl_epb()
int	sk_stati()

begin
	switch (sk_stati (coo, S_CTYPE)) {

	case CTYPE_EQUATORIAL:

	    switch (sk_stati(coo, S_RADECSYS)) {

	    case EQTYPE_GAPPT:
		if (IS_INDEFD(sk_statd(coo, S_EPOCH))) {
	            call sprintf (wcsstr, maxch, "apparent")
		} else {
	            call sprintf (wcsstr, maxch, "apparent J%0.8f")
		        call pargd (sl_epj(sk_statd(coo, S_EPOCH)))
		}

	    case EQTYPE_FK5:
	        call sprintf (wcsstr, maxch, "fk5 J%0.3f J%0.8f")
		    call pargd (sk_statd(coo, S_EQUINOX))
		    call pargd (sl_epj(sk_statd(coo, S_EPOCH)))

	    case EQTYPE_ICRS:
	        call sprintf (wcsstr, maxch, "icrs J%0.3f J%0.8f")
		    call pargd (sk_statd(coo, S_EQUINOX))
		    call pargd (sl_epj(sk_statd(coo, S_EPOCH)))

	    case EQTYPE_FK4:
	        call sprintf (wcsstr, maxch, "fk4 B%0.3f B%0.8f")
		    call pargd (sk_statd(coo, S_EQUINOX))
		    call pargd (sl_epb(sk_statd(coo, S_EPOCH)))

	    case EQTYPE_FK4NOE:
	        call sprintf (wcsstr, maxch, "fk4noe B%0.3f B%0.8f")
		    call pargd (sk_statd(coo, S_EQUINOX))
		    call pargd (sl_epb(sk_statd(coo, S_EPOCH)))

	    default:
		wcsstr[1] = EOS
	    }

	case CTYPE_ECLIPTIC:
	    if (IS_INDEFD(sk_statd(coo, S_EPOCH))) {
	        call sprintf (wcsstr, maxch, "ecliptic")
	    } else {
	        call sprintf (wcsstr, maxch, "ecliptic J%0.8f")
		    call pargd (sl_epj(sk_statd(coo, S_EPOCH)))
	    }

	case CTYPE_GALACTIC:
	    call sprintf (wcsstr, maxch, "galactic J%0.8f")
		call pargd (sl_epj(sk_statd(coo, S_EPOCH)))

	case CTYPE_SUPERGALACTIC:
	    call sprintf (wcsstr, maxch, "supergalactic j%0.8f")
		call pargd (sl_epj(sk_statd(coo, S_EPOCH)))
	}
end


# SK_COPY -- Copy the coodinate structure.

pointer procedure sk_copy (cooin)

pointer	cooin			#I the pointer to the input structure

pointer	cooout

begin
	if (cooin == NULL)
	    cooout = NULL
	else {
	    call calloc (cooout, LEN_SKYCOOSTRUCT, TY_STRUCT)
            SKY_VXOFF(cooout) = SKY_VXOFF(cooin)
            SKY_VYOFF(cooout) = SKY_VYOFF(cooin)
            SKY_VXSTEP(cooout) = SKY_VXSTEP(cooin)
            SKY_VYSTEP(cooout) = SKY_VYSTEP(cooin)
	    SKY_EQUINOX(cooout) = SKY_EQUINOX(cooin)
	    SKY_EPOCH(cooout) = SKY_EPOCH(cooin)
	    SKY_CTYPE(cooout) = SKY_CTYPE(cooin)
	    SKY_RADECSYS(cooout) = SKY_RADECSYS(cooin)
            SKY_WTYPE(cooout) = SKY_WTYPE(cooin)
            SKY_PLNGAX(cooout) = SKY_PLNGAX(cooin)
            SKY_PLATAX(cooout) = SKY_PLATAX(cooin)
            SKY_XLAX(cooout) = SKY_XLAX(cooin)
            SKY_YLAX(cooout) = SKY_YLAX(cooin)
            SKY_PIXTYPE(cooout) = SKY_PIXTYPE(cooin)
    	    SKY_NLNGAX(cooout) = SKY_NLNGAX(cooin)
            SKY_NLATAX(cooout) = SKY_NLATAX(cooin)
    	    SKY_NLNGUNITS(cooout) = SKY_NLNGUNITS(cooin)
            SKY_NLATUNITS(cooout) = SKY_NLATUNITS(cooin)
	    call strcpy (SKY_COOSYSTEM(cooin), SKY_COOSYSTEM(cooout),
		SZ_FNAME)
	}

	return (cooout)
end


# SK_CLOSE -- Free the coordinate structure.

procedure sk_close (coo)

pointer	coo			#U the input coordinate structure

begin
	if (coo != NULL)
	    call mfree (coo, TY_STRUCT)
end
