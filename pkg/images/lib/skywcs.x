include <imhdr.h>
include <imio.h>
include <math.h>
include <mwset.h>
include "skywcs.h"
include "skywcsdef.h"

define SZ_DMYTOKEN	2

# SK_DECWCS -- Decode the wcs string which may be either an image name
# plus wcs, e.g. "dev$pix logical" or a string describing the celestial
# coordinate system, e.g. "J2000" or "galactic" into a celestial coordinate
# strucuture. If the input wcs is an image wcs then a non-NULL pointer to
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
	    SKY_PTYPE(coo) = PIXTYPE_WORLD
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


# SK_DECIM -- Given an image descriptor and an image wcs string create a
# celstial coordinate structure. ERR is returned if the image wcs cannot be
# decoded or a a valid celestial coordinate descriptor cannot be created.

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
	    SKY_PTYPE(coo) = PIXTYPE_LOGICAL
	    SKY_NLNGUNITS(coo) = SKY_DEGREES
	    SKY_NLATUNITS(coo) = SKY_DEGREES
	    stat = ERR

	# Decode the wcs.
	} else {
	    SKY_PTYPE(coo) = strdic (wcs, Memc[str1], SZ_LINE, PIXTYPE_LIST)
	    if (SKY_PTYPE(coo) <= 0)
	        SKY_PTYPE(coo) = PIXTYPE_LOGICAL
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
# The string syntax is [ctype] equinox [epoch].

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

	# Decode the ra/dec system and equinox
	switch (sctype) {

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
		    #epoch = equinox
		    epoch = sl_eb2d (equinox)
		else {
		    if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		        Memc[str2] == 'B' || Memc[str2] == 'b')
		        ip = 2
		    else
		        ip = 1
		    if (ctod (Memc[str2], ip, epoch) <= 0)
		        #epoch = equinox
		        epoch = sl_eb2d (equinox)
		    else if (epoch <= 3000.0d0 && (Memc[str2] == 'J' ||
		        Memc[str2] == 'j'))
		        #epoch = sl_epb (sl_ej2d (epoch))
		        epoch = sl_ej2d (epoch)
		    else if (epoch > 3000.0d0)
		        #epoch = sl_epb (epoch - 2400000.5d0)
		        epoch = epoch - 2400000.5d0
		    else
			epoch = sl_eb2d (epoch)
		}

	    case EQTYPE_FK5:
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
		    #epoch = equinox
		    epoch = sl_ej2d (equinox)
		else {
		    if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		        Memc[str2] == 'B' || Memc[str2] == 'b')
		        ip = 2
		    else
		        ip = 1
		    if (ctod (Memc[str2], ip, epoch) <= 0)
		        #epoch = equinox
		        epoch = sl_ej2d (equinox)
		    else if (epoch <= 3000.0d0 && (Memc[str2] == 'B' ||
		        Memc[str2] == 'b'))
		        #epoch = sl_epj (sl_eb2d (epoch))
		        epoch = sl_eb2d (epoch)
		    else if (epoch > 3000.0d0)
		        #epoch = sl_epj (epoch - 2400000.5d0)
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
			#epoch = equinox
		        epoch = sl_eb2d (equinox)
		    else {
		        if (Memc[str2] == 'J' || Memc[str2] == 'j')
		            ip = 2
		        else if (Memc[str2] == 'B' || Memc[str2] == 'b')
		            ip = 2
		        else
		            ip = 1
		        if (ctod (Memc[str2], ip, epoch) <= 0)
		            #epoch = equinox
		            epoch = sl_eb2d (equinox)
		        else if (epoch <= 3000.0d0 && (Memc[str2] == 'J' ||
			    Memc[str2] == 'j'))
		            #epoch = sl_epb (sl_ej2d (epoch))
		            epoch = sl_ej2d (epoch)
		        else if (epoch > 3000.0d0)
		            #epoch = sl_epb (epoch - 2400000.5d0)
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
		        #epoch = equinox
		        epoch = sl_ej2d (equinox)
		    else {
		        if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		            Memc[str2] == 'B' || Memc[str2] == 'b')
		            ip = 2
		        else
		            ip = 1
		        if (ctod (Memc[str2], ip, epoch) <= 0)
		            #epoch = equinox
		            epoch = sl_ej2d (equinox)
		        else if (epoch <= 3000.0d0 && (Memc[str2] == 'B' ||
			    Memc[str2] == 'b'))
		            #epoch = sl_epj (sl_eb2d (epoch))
		            epoch = sl_eb2d (epoch)
		        else if (epoch > 3000.0d0)
		            #epoch = sl_epj (epoch - 2400000.5d0)
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
		        #epoch = equinox
		        epoch = sl_eb2d (equinox)
		    else {
		        if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		            Memc[str2] == 'B' || Memc[str2] == 'b')
		            ip = 2
		        else
		            ip = 1
		        if (ctod (Memc[str2], ip, epoch) <= 0)
		            #epoch = equinox
		            epoch = sl_eb2d (equinox)
		        else if (epoch <= 3000.0d0 && (Memc[str2] == 'J' ||
			    Memc[str2] == 'j'))
		            #epoch = sl_epb (sl_ej2d (epoch))
		            epoch = sl_ej2d (epoch)
		        else if (epoch > 3000.0d0)
		            #epoch = sl_epb (epoch - 2400000.5d0)
		            epoch = epoch - 2400000.5d0
			else
			    epoch = sl_eb2d (epoch)
		    }

		} else {
		    radecsys = EQTYPE_FK5
	            call gargwrd (Memc[str2], SZ_LINE)
	            if (nscan() <= nitems)
		        #epoch = equinox
		        epoch = sl_ej2d (equinox)
		    else {
		        if (Memc[str2] == 'J' || Memc[str2] == 'j' ||
		            Memc[str2] == 'B' || Memc[str2] == 'b')
		            ip = 2
		        else
		            ip = 1
		        if (ctod (Memc[str2], ip, epoch) <= 0)
		            #epoch = equinox
		            epoch = sl_ej2d (equinox)
		        else if (epoch <= 3000.0d0 && (Memc[str2] == 'B' ||
			    Memc[str2] == 'b'))
		            #epoch = sl_epj (sl_eb2d (epoch))
		            epoch = sl_eb2d (epoch)
		        else if (epoch > 3000.0d0)
		            #epoch = sl_epj (epoch - 2400000.5d0)
		            epoch = epoch - 2400000.5d0
			else
			    epoch = sl_ej2d (epoch)
		    }
		}
	    }

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
	        else if (epoch < 1980.0d0) 
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

int	i, ndim, axtype, token, day, month, year, ier
pointer	sp, atval
double	imgetd(), sl_eb2d(), sl_ej2d()
int	mw_stati(), strdic()
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
		    } else {
		        call sscan (Memc[atval])
			    call gargi (day)
			    call gargtok (token, Memc[atval], SZ_DMYTOKEN)
			    call gargi (month)
			    call gargtok (token, Memc[atval], SZ_DMYTOKEN)
			    call gargi (year)
		        call sl_cadj (year, month, day, epoch, ier)
		        if (ier != 0)
			    epoch = INDEFD
		    }
	        }
	    }

	    # Set the default equinox and epoch appropriate for each
	    # equatorial system if these are undefined.
	    switch (radecsys) {
	    case EQTYPE_FK4:
		if (IS_INDEFD(equinox))
		    equinox = 1950.0d0
		if (IS_INDEFD(epoch))
		    #epoch = 1950.0d0
		    epoch = sl_eb2d (1950.0d0)
		#else
		    #epoch = sl_epb (epoch)
	    case EQTYPE_FK4NOE:
		if (IS_INDEFD(equinox))
		    equinox = 1950.0d0
		if (IS_INDEFD(epoch))
		    #epoch = 1950.0d0
		    epoch = sl_eb2d (1950.0d0)
		#else
		    #epoch = sl_epb (epoch)
	    case EQTYPE_FK5:
		if (IS_INDEFD(equinox))
		    equinox = 2000.0d0
		if (IS_INDEFD(epoch))
		    #epoch = 2000.0d0
		    epoch = sl_ej2d (2000.0d0)
		#else
		    #epoch = sl_epj (epoch)
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
		    } else {
		        call sscan (Memc[atval])
			    call gargi (day)
			    call gargtok (token, Memc[atval], SZ_DMYTOKEN)
			    call gargi (month)
			    call gargtok (token, Memc[atval], SZ_DMYTOKEN)
			    call gargi (year)
		        call sl_cadj (year, month, day, epoch, ier)
		        if (ier != 0)
			    epoch = INDEFD
		    }
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
		    } else {
		        call sscan (Memc[atval])
			    call gargi (day)
			    call gargtok (token, Memc[atval], SZ_DMYTOKEN)
			    call gargi (month)
			    call gargtok (token, Memc[atval], SZ_DMYTOKEN)
			    call gargi (year)
		        call sl_cadj (year, month, day, epoch, ier)
		        if (ier != 0)
			    epoch = sl_eb2d (1950.0d0)
			else if (epoch < 1984.0d0)
		    	    epoch = sl_eb2d (epoch)
			else
		    	    epoch = sl_ej2d (epoch)
		    }
	        }
	    }
	}

	call sfree (sp)

	return (OK)
end


# SK_CLOSE -- Free the coordinate structure.

procedure sk_close (coo)

pointer	coo			#I the input coordinate structure

begin
	if (coo != NULL)
	    call mfree (coo, TY_STRUCT)
end


# SK_CSCOPY -- Copy the coodinate structure.

pointer procedure sk_cscopy (cooin)

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
            SKY_PTYPE(cooout) = SKY_PTYPE(cooin)
    	    SKY_NLNGAX(cooout) = SKY_NLNGAX(cooin)
            SKY_NLATAX(cooout) = SKY_NLATAX(cooin)
    	    SKY_NLNGUNITS(cooout) = SKY_NLNGUNITS(cooin)
            SKY_NLATUNITS(cooout) = SKY_NLATUNITS(cooin)
	    call strcpy (SKY_COOSYSTEM(cooin), SKY_COOSYSTEM(cooout),
		SZ_FNAME)
	}

	return (cooout)
end

# SKY_HDRSAVEIM -- Update the image header keywords that describe the
# fundamental coordinate system, CTYPE, RADECSYS, EQUINOX (EPOCH), and
# MJD-WCS.

procedure sk_hdrsaveim (coo, mw, im)

pointer	coo			#I pointer to the coordinate structure
pointer	mw			#I pointer to the mwcs structure
pointer	im			#I image descriptor

errchk	imdelf()

begin
	# Move all this to a separate routine
	switch (SKY_CTYPE(coo)) {

	case CTYPE_EQUATORIAL:
	    call mw_swattrs (mw, SKY_PLNGAX(coo), "axtype", "ra")
	    call mw_swattrs (mw, SKY_PLATAX(coo), "axtype", "dec")
	    switch (SKY_RADECSYS(coo)) {
	    case EQTYPE_FK4:
		call imastr (im, "radecsys", "FK4")
		call imaddd (im, "equinox", SKY_EQUINOX(coo))
		#iferr (call imdelf (im, "epoch"))
		    #;
		#call imaddd (im, "mjd-wcs", sl_eb2d (SKY_EPOCH(coo)))
		call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))
	    case EQTYPE_FK4NOE:
		call imastr (im, "radecsys", "FK4NOE")
		call imaddd (im, "equinox", SKY_EQUINOX(coo))
		#iferr (call imdelf (im, "epoch"))
		    #;
		#call imaddd (im, "mjd-wcs", sl_eb2d (SKY_EPOCH(coo)))
		call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))
	    case EQTYPE_FK5:
		call imastr (im, "radecsys", "FK5")
		call imaddd (im, "equinox", SKY_EQUINOX(coo))
		#iferr (call imdelf (im, "epoch"))
		    #;
	        iferr (call imdelf (im, "mjd-wcs"))
		    ;
		#call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))
	    case EQTYPE_GAPPT:
		call imastr (im, "radecsys", "GAPPT")
		iferr (call imdelf (im, "equinox"))
		    ;
		#iferr (call imdelf (im, "epoch"))
		    #;
		call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))
	    }

	case CTYPE_ECLIPTIC:
	    call mw_swattrs (mw, SKY_PLNGAX(coo), "axtype", "elon")
	    call mw_swattrs (mw, SKY_PLATAX(coo), "axtype", "elat")
	    iferr (call imdelf (im, "radecsys"))
		;
	    iferr (call imdelf (im, "equinox"))
		;
	    #iferr (call imdelf (im, "epoch"))
		#;
	    call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))

	case CTYPE_GALACTIC:
	    call mw_swattrs (mw, SKY_PLNGAX(coo), "axtype", "glon")
	    call mw_swattrs (mw, SKY_PLATAX(coo), "axtype", "glat")
	    iferr (call imdelf (im, "radecsys"))
		;
	    iferr (call imdelf (im, "equinox"))
		;
	    #iferr (call imdelf (im, "epoch"))
		#;
	    iferr (call imdelf (im, "mjd-wcs"))
		;
	    #call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))

	case CTYPE_SUPERGALACTIC:
	    call mw_swattrs (mw, SKY_PLNGAX(coo), "axtype", "slon")
	    call mw_swattrs (mw, SKY_PLATAX(coo), "axtype", "slat")
	    iferr (call imdelf (im, "radecsys"))
		;
	    iferr (call imdelf (im, "equinox"))
		;
	    #iferr (call imdelf (im, "epoch"))
		#;
	    iferr (call imdelf (im, "mjd-wcs"))
		;
	    #call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))
	}
end


# SK_CTYPEIM -- Modify the CTYPE keywords appropriately. This step will
# become unnecessary when MWCS is updated to deal with non-equatorial celestial
# coordinate systems.

procedure sk_ctypeim (coo, im)

pointer	coo			#I pointer to the coordinate structure
pointer	im			#I image descriptor

pointer	sp, wtype, key1, key2, attr
int	rg_wrdstr()

begin
	call smark (sp)
	call salloc (key1, 8, TY_CHAR)
	call salloc (key2, 8, TY_CHAR)
	call salloc (wtype, 3, TY_CHAR)
	call salloc (attr, 8, TY_CHAR)

	call sprintf (Memc[key1], 8, "CTYPE%d")
	    call pargi (SKY_PLNGAX(coo))
	call sprintf (Memc[key2], 8, "CTYPE%d")
	    call pargi (SKY_PLATAX(coo))

	if (SKY_WTYPE(coo) <= 0 || SKY_WTYPE(coo) == WTYPE_LIN) {
	    call imastr (im, Memc[key1], "LINEAR")
	    call imastr (im, Memc[key2], "LINEAR")
	    call sfree (sp)
	    return
	}

	if (rg_wrdstr (SKY_WTYPE(coo), Memc[wtype], 3, WTYPE_LIST) <= 0)
	    call strcpy ("tan", Memc[wtype], 3)
	call strupr (Memc[wtype])

	# Move all this to a separate routine
	switch (SKY_CTYPE(coo)) {

	case CTYPE_EQUATORIAL:
	    call sprintf (Memc[attr], 8, "RA---%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key1], Memc[attr])
	    call sprintf (Memc[attr], 8, "DEC--%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key2], Memc[attr])

	case CTYPE_ECLIPTIC:
	    call sprintf (Memc[attr], 8, "ELON-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key1], Memc[attr])
	    call sprintf (Memc[attr], 8, "ELAT-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key2], Memc[attr])

	case CTYPE_GALACTIC:
	    call sprintf (Memc[attr], 8, "GLON-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key1], Memc[attr])
	    call sprintf (Memc[attr], 8, "GLAT-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key2], Memc[attr])

	case CTYPE_SUPERGALACTIC:
	    call sprintf (Memc[attr], 8, "SLON-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key1], Memc[attr])
	    call sprintf (Memc[attr], 8, "SLAT-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key2], Memc[attr])

	default:
	    call imastr (im, Memc[key1], "LINEAR")
	    call imastr (im, Memc[key2], "LINEAR")
	}

	call sfree (sp)
end


# SK_STATD -- Get a double precision coordinate parameter.

double procedure sk_statd (coo, param)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter

begin
	switch (param) {
	case S_VXOFF:
	    return (SKY_VXOFF(coo))
	case S_VYOFF:
	    return (SKY_VYOFF(coo))
	case S_VXSTEP:
	    return (SKY_VXSTEP(coo))
	case S_VYSTEP:
	    return (SKY_VYSTEP(coo))
	case S_EQUINOX:
	    return (SKY_EQUINOX(coo))
	case S_EPOCH:
	    return (SKY_EPOCH(coo))
	default:
	    call error (0, "SKY_STATD: Unknown coordinate system parameter")
	}
end


# SK_SETD -- Set a double precision coordinate parameter.

procedure sk_setd (coo, param, value)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter
double	value			#I the parameter value

begin
	switch (param) {
	case S_VXOFF:
	    SKY_VXOFF(coo) = value
	case S_VYOFF:
	    SKY_VYOFF(coo) = value
	case S_VXSTEP:
	    SKY_VXSTEP(coo) = value
	case S_VYSTEP:
	    SKY_VYSTEP(coo) = value
	case S_EQUINOX:
	    SKY_EQUINOX(coo) = value
	case S_EPOCH:
	    SKY_EPOCH(coo) = value
	default:
	    call error (0, "SKY_SETD: Unknown coordinate system parameter")
	}
end


# SK_STATI -- Get an integer coordinate parameter.

int procedure sk_stati (coo, param)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter

begin
	switch (param) {
	case S_CTYPE:
	    return (SKY_CTYPE(coo))
	case S_RADECSYS:
	    return (SKY_RADECSYS(coo))
	case S_WTYPE:
	    return (SKY_WTYPE(coo))
	case S_PLNGAX:
	    return (SKY_PLNGAX(coo))
	case S_PLATAX:
	    return (SKY_PLATAX(coo))
	case S_XLAX:
	    return (SKY_XLAX(coo))
	case S_YLAX:
	    return (SKY_YLAX(coo))
	case S_PTYPE:
	    return (SKY_PTYPE(coo))
	case S_NLNGAX:
	    return (SKY_NLNGAX(coo))
	case S_NLATAX:
	    return (SKY_NLATAX(coo))
	case S_NLNGUNITS:
	    return (SKY_NLNGUNITS(coo))
	case S_NLATUNITS:
	    return (SKY_NLATUNITS(coo))
	case S_STATUS:
	    return (SKY_STATUS(coo))
	default:
	    call error (0, "SKY_STATI: Unknown coordinate system parameter")
	}
end


# SK_SETI -- Set an integer coordinate parameter.

procedure sk_seti (coo, param, value)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter
int	value			#I the parameter value

begin
	switch (param) {
	case S_CTYPE:
	    SKY_CTYPE(coo) = value
	case S_RADECSYS:
	    SKY_RADECSYS(coo) = value
	case S_WTYPE:
	    SKY_WTYPE(coo) = value
	case S_PLNGAX:
	    SKY_PLNGAX(coo) = value
	case S_PLATAX:
	    SKY_PLATAX(coo) = value
	case S_XLAX:
	    SKY_XLAX(coo) = value
	case S_YLAX:
	    SKY_YLAX(coo) = value
	case S_PTYPE:
	    SKY_PTYPE(coo) = value
	case S_NLNGAX:
	    SKY_NLNGAX(coo) = value
	case S_NLATAX:
	    SKY_NLATAX(coo) = value
	case S_NLNGUNITS:
	    SKY_NLNGUNITS(coo) = value
	case S_NLATUNITS:
	    SKY_NLATUNITS(coo) = value
	case S_STATUS:
	    SKY_STATUS(coo) = value
	default:
	    call error (0, "SKY_SETI: Unknown coordinate system parameter")
	}
end


# SK_GETSTR -- Get a character string coordinate parameter.

procedure sk_getstr (coo, param, value, maxch)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter
char	value			#O the output string
int	maxch			#I the maximum size of the string

begin
	switch (param) {
	case S_COOSYSTEM:
	    call strcpy (SKY_COOSYSTEM(coo), value, maxch)
	default:
	    call error (0, "SKY_GETSTR: Unknown coordinate system parameter")
	}
end


# SK_SETSTR -- Set a character string coordinate parameter.

procedure sk_setstr (coo, param, value)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter
char	value[ARB]		#I the parameter value

begin
	switch (param) {
	case S_COOSYSTEM:
	    call strcpy (value, SKY_COOSYSTEM(coo), SZ_FNAME)
	default:
	    call error (0, "SKY_SETSTR: Unknown coordinate system parameter")
	}
end


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
	        SKY_PLATAX(coo), SKY_WTYPE(coo), SKY_PTYPE(coo),
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
		SKY_PTYPE(coo), SKY_RADECSYS(coo), SKY_EQUINOX(coo),
		SKY_EPOCH(coo))
end


# SK_INPRINT -- Print a summary of the input list coordinate system.

procedure sk_inprint (label, system, ctype, radecsys, equinox, epoch)

char	label[ARB]		#I the input label
char	system[ARB]		#I the input system
int	ctype			#I the input coordinate type
int	radecsys		#I the input equatorial reference system
double	equinox			#I the input equinox
double	epoch			#I the input epoch of the observation

pointer	sp, radecstr
double	sl_epj(), sl_epb()
int	rg_wrdstr()

begin
	call smark (sp)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)

	switch (ctype) {

	case CTYPE_EQUATORIAL:
	    if (rg_wrdstr (radecsys, Memc[radecstr], SZ_FNAME,
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
	    case EQTYPE_FK5:
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
int	rg_wrdstr()

begin
	call smark (sp)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)

	switch (ctype) {

	case CTYPE_EQUATORIAL:
	    if (rg_wrdstr (radecsys, Memc[radecstr], SZ_FNAME,
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
	    case EQTYPE_FK5:
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
int	rg_wrdstr()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (projstr, SZ_FNAME, TY_CHAR)
	call salloc (wcsstr, SZ_FNAME, TY_CHAR)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)

	call sscan (imagesys)
	    call gargwrd (Memc[imname], SZ_FNAME)
	if (rg_wrdstr (wtype, Memc[projstr], SZ_FNAME, WTYPE_LIST) <= 0)
	    call strcpy ("linear", Memc[projstr], SZ_FNAME)
	call strupr (Memc[projstr])
	if (rg_wrdstr (ptype, Memc[wcsstr], SZ_FNAME, PIXTYPE_LIST) <= 0)
	    call strcpy ("world", Memc[wcsstr], SZ_FNAME)
	call strlwr (Memc[wcsstr])

	switch (ctype) {

	case CTYPE_EQUATORIAL:
	    if (rg_wrdstr (radecsys, Memc[radecstr], SZ_FNAME,
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
	    case EQTYPE_FK5:
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
int	rg_wrdstr()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (projstr, SZ_FNAME, TY_CHAR)
	call salloc (wcsstr, SZ_FNAME, TY_CHAR)
	call salloc (radecstr, SZ_FNAME, TY_CHAR)

	call sscan (imagesys)
	    call gargwrd (Memc[imname], SZ_FNAME)
	if (rg_wrdstr (wtype, Memc[projstr], SZ_FNAME, WTYPE_LIST) <= 0)
	    call strcpy ("linear", Memc[projstr], SZ_FNAME)
	call strupr (Memc[projstr])
	if (rg_wrdstr (ptype, Memc[wcsstr], SZ_FNAME, PIXTYPE_LIST) <= 0)
	    call strcpy ("world", Memc[wcsstr], SZ_FNAME)
	call strlwr (Memc[wcsstr])

	switch (ctype) {

	case CTYPE_EQUATORIAL:
	    if (rg_wrdstr (radecsys, Memc[radecstr], SZ_FNAME,
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
	    case EQTYPE_FK5:
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


# SK_ULTRAN -- Transform the sky coordinates from the input coordinate
# system to the output coordinate system using the units conversions as
# appropriate.

procedure sk_ultran (cooin, cooout, ilng, ilat, olng, olat, npts) 

pointer	cooin		#I pointer to the input coordinate system structure
pointer	cooout		#I pointer to the output coordinate system structure
double	ilng[ARB]	#I the input ra/longitude in radians
double	ilat[ARB]	#I the input dec/latitude in radians
double	olng[ARB]	#I the output ra/longitude in radians
double	olat[ARB]	#I the output dec/latitude in radians
int	npts		#I the number of points to be converted

double	tilng, tilat, tolng, tolat
int	i

begin
	do i = 1, npts {

	    switch (SKY_NLNGUNITS(cooin)) {
	    case SKY_HOURS:
		tilng = DEGTORAD(15.0d0 * ilng[i])
	    case SKY_DEGREES:
		tilng = DEGTORAD(ilng[i])
	    case SKY_RADIANS:
		tilng = ilng[i]
	    default:
		tilng = ilng[i]
	    }
	    switch (SKY_NLATUNITS(cooin)) {
	    case SKY_HOURS:
		tilat = DEGTORAD(15.0d0 * ilat[i])
	    case SKY_DEGREES:
		tilat = DEGTORAD(ilat[i])
	    case SKY_RADIANS:
		tilat = ilat[i]
	    default:
		tilat = ilat[i]
	    }

	    call sk_lltran (cooin, cooout, tilng, tilat, INDEFD, INDEFD,
		0.0d0, 0.0d0, tolng, tolat)

	    switch (SKY_NLNGUNITS(cooout)) {
	    case SKY_HOURS:
		olng[i] = RADTODEG(tolng) / 15.0d0
	    case SKY_DEGREES:
		olng[i] = RADTODEG(tolng)
	    case SKY_RADIANS:
		olng[i] = tolng
	    default:
		olng[i] = tolng
	    }
	    switch (SKY_NLATUNITS(cooout)) {
	    case SKY_HOURS:
		olat[i] = RADTODEG(tolat) / 15.0d0
	    case SKY_DEGREES:
		olat[i] = RADTODEG(tolat)
	    case SKY_RADIANS:
		olat[i] = tolat
	    default:
		olat[i] = tolat
	    }
	}
end


# SK_LLTRAN -- Transform the sky coordinate from the input coordinate
# system to the output coordinate system assuming that all the coordinate
# are in radians.

procedure sk_lltran (cooin, cooout, ilng, ilat, ipmlng, ipmlat, px, rv,
	olng, olat)

pointer	cooin		#I pointer to the input coordinate system structure
pointer	cooout		#I pointer to the output coordinate system structure
double	ilng		#I the input ra/longitude in radians
double	ilat		#I the input dec/latitude in radians
double	ipmlng		#I the input proper motion in ra in radians
double	ipmlat		#I the input proper motion in dec in radians
double	px		#I the input parallax in arcseconds
double	rv		#I the input radial velocity in km / second
double	olng		#I the output ra/longitude in radians
double	olat		#I the output dec/latitude in radians

int	pmflag
double	pmr, pmd
double	sl_epj(), sl_epb()

begin
	# Test for the case where the input coordinate system is the
	# same as the output coordinate system.
	if (SKY_CTYPE(cooin) == SKY_CTYPE(cooout)) {

	    switch (SKY_CTYPE(cooin)) {

	    case CTYPE_EQUATORIAL:
		call sk_equatorial (cooin, cooout, ilng, ilat, ipmlng,
		    ipmlat, px, rv, olng, olat)

	    case CTYPE_ECLIPTIC:
		if (SKY_EPOCH(cooin) == SKY_EPOCH(cooout)) {
		    olng = ilng
		    olat = ilat
		} else {
		    call sl_eceq (ilng, ilat, SKY_EPOCH(cooin), olng, olat)
		    call sl_eqec (olng, olat, SKY_EPOCH(cooout), olng, olat)
		}

	    default:
		olng = ilng
		olat = ilat
	    }

	    return
	}

	# Compute proper motions ?
	if (! IS_INDEFD(ipmlng) && ! IS_INDEFD(ipmlat))
	    pmflag = YES
	else
	    pmflag = NO

	# Cover the remaining cases.
	switch (SKY_CTYPE(cooin)) {

	# The input system is equatorial.
	case CTYPE_EQUATORIAL:

	    switch (SKY_RADECSYS(cooin)) {

	    case EQTYPE_FK4, EQTYPE_FK4NOE:
	        if (pmflag == YES) {
		    call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		        sl_epb (SKY_EPOCH(cooin)), sl_epb (SKY_EPOCH(cooout)),
			olng, olat)
	        } else {
		    olng = ilng
		    olat = ilat
	        }
		if (SKY_RADECSYS(cooin) == EQTYPE_FK4)
		    call sl_suet (olng, olat, SKY_EQUINOX(cooin), olng, olat)
		if (SKY_EQUINOX(cooin) != 1950.0d0)
		    call sl_prcs (1, SKY_EQUINOX(cooin), 1950.0d0, olng, olat) 
		call sl_adet (olng, olat, 1950.0d0, olng, olat)
		if (pmflag == YES)
		    call sl_f45z (olng, olat, sl_epb(SKY_EPOCH(cooout)),
		        olng, olat)
		else
		    call sl_f45z (olng, olat, sl_epb (SKY_EPOCH(cooin)),
		        olng, olat)

	    case EQTYPE_FK5:
	        if (pmflag == YES) {
		    call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		        sl_epj (SKY_EPOCH(cooin)), sl_epj(SKY_EPOCH(cooout)),
			olng, olat)
	        } else {
	            olng = ilng
	            olat = ilat
		}
		if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 

	    case EQTYPE_GAPPT:
		call sl_amp (ilng, ilat, SKY_EPOCH(cooin), 2000.0d0, olng, olat)

	    }

	    switch (SKY_CTYPE(cooout)) {

	    # The output coordinate system is ecliptic.
	    case CTYPE_ECLIPTIC:
		call sl_eqec (olng, olat, SKY_EPOCH(cooout), olng, olat)

	    # The output coordinate system is galactic.
	    case CTYPE_GALACTIC:
		call sl_eqga (olng, olat, olng, olat)

	    # The output coordinate system is supergalactic.
	    case CTYPE_SUPERGALACTIC:
		call sl_eqga (olng, olat, olng, olat)
		call sl_gasu (olng, olat, olng, olat)

	    default:
	        olng = ilng
	        olat = ilat
	    }

	# The input coordinate system is ecliptic.
	case CTYPE_ECLIPTIC:

	    call sl_eceq (ilng, ilat, SKY_EPOCH(cooin), olng, olat)
	    switch (SKY_CTYPE(cooout)) {

	    # The output coordinate system is equatorial.
	    case CTYPE_EQUATORIAL:
		#call sl_eceq (ilng, ilat, SKY_EPOCH(cooin), olng, olat)
		switch (SKY_RADECSYS(cooout)) {
		case EQTYPE_FK4, EQTYPE_FK4NOE:
		    call sl_f54z (olng, olat, sl_epb(SKY_EPOCH(cooout)),
		        olng, olat, pmr, pmd)
		    call sl_suet (olng, olat, 1950.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 1950.0d0)
			call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		    if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
		        call sl_adet (olng, olat, SKY_EQUINOX(cooout),
			    olng, olat)
		case EQTYPE_FK5:
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		case EQTYPE_GAPPT:
		    call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0,
			2000.0d0, SKY_EPOCH(cooout), olng, olat)
		}

	    # The output coordinate system is galactic.
	    case CTYPE_GALACTIC:
		#call sl_eceq (ilng, ilat, SKY_EPOCH(cooin), olng, olat)
		call sl_eqga (olng, olat, olng, olat)

	    # The output system is supergalactic.
	    case CTYPE_SUPERGALACTIC:
		#call sl_eceq (ilng, ilat, SKY_EPOCH(cooin), olng, olat)
		call sl_eqga (olng, olat, olng, olat)
		call sl_gasu (olng, olat, olng, olat)

	    default:
	        olng = ilng
	        olat = ilat
	    }

	# The input coordinate system is galactic.
	case CTYPE_GALACTIC:

	    switch (SKY_CTYPE(cooout)) {

	    # The output coordinate system is equatorial.
	    case CTYPE_EQUATORIAL:
	        call sl_gaeq (ilng, ilat, olng, olat)
		switch (SKY_RADECSYS(cooout)) {
		case EQTYPE_FK4, EQTYPE_FK4NOE:
		    call sl_f54z (olng, olat, sl_epb(SKY_EPOCH(cooout)),
		        olng, olat, pmr, pmd)
		    call sl_suet (olng, olat, 1950.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 1950.0d0)
			call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		    if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
		        call sl_adet (olng, olat, SKY_EQUINOX(cooout),
			    olng, olat)
		case EQTYPE_FK5:
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		case EQTYPE_GAPPT:
		    call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0,
			2000.0d0, SKY_EPOCH(cooout), olng, olat)
		}

	    # The output coordinate system is ecliptic.
	    case CTYPE_ECLIPTIC:
		call sl_gaeq (ilng, ilat, olng, olat)
		call sl_eqec (olng, olat, SKY_EPOCH(cooout), olng, olat)

	    # The output coordinate system is supergalactic.
	    case CTYPE_SUPERGALACTIC:
		call sl_gasu (ilng, ilat, olng, olat)

	    default:
	        olng = ilng
	        olat = ilat
	    }

	# The input coordinates are supergalactic.
	case CTYPE_SUPERGALACTIC:

	    switch (SKY_CTYPE(cooout)) {

	    case CTYPE_EQUATORIAL:
		call sl_suga (ilng, ilat, olng, olat)
		switch (SKY_RADECSYS(cooout)) {
		case EQTYPE_FK4:
		    call sl_gaeq (olng, olat, olng, olat)
		    call sl_f54z (olng, olat, sl_epb (SKY_EPOCH(cooout)),
		        olng, olat, pmr, pmd)
		    call sl_suet (olng, olat, 1950.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 1950.0d0)
			call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		    call sl_adet (olng, olat, SKY_EQUINOX(cooout), olng, olat)
		case EQTYPE_FK4NOE:
		    call sl_gaeq (olng, olat, olng, olat)
		    call sl_f54z (olng, olat, sl_epb (SKY_EPOCH(cooout)),
		        olng, olat, pmr, pmd)
		    call sl_suet (olng, olat, 1950.0d0, olng, olat)
		    if (SKY_EQUINOX(cooout) != 1950.0d0)
			call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		case EQTYPE_FK5:
		    call sl_gaeq (olng, olat, olng, olat)
		    if (SKY_EQUINOX(cooout) != 2000.0d0)
			call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout),
			    olng, olat) 
		case EQTYPE_GAPPT:
		    call sl_gaeq (olng, olat, olng, olat)
		    call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0,
			2000.0d0, SKY_EPOCH(cooout), olng, olat)
		}

	    case CTYPE_ECLIPTIC:
		call sl_suga (ilng, ilat, olng, olat)
		call sl_gaeq (olng, olat, olng, olat)
		call sl_eqec (olng, olat, SKY_EPOCH(cooout), olng, olat)

	    case CTYPE_GALACTIC:
		call sl_suga (ilng, ilat, olng, olat)

	    default:
	        olng = ilng
	        olat = ilat
	    }

	default:
	    olng = ilng
	    olat = ilat
	}
end


# SK_EQUATORIAL -- Convert / precess equatorial coordinates.

procedure sk_equatorial (cooin, cooout, ilng, ilat, ipmlng, ipmlat,
	px, rv, olng, olat)

pointer	cooin		#I the input coordinate system structure
pointer	cooout		#I the output coordinate system structure
double	ilng		#I the input ra in radians
double	ilat		#I the input dec in radians
double	ipmlng		#I the input proper motion in ra in radians
double	ipmlat		#I the input proper motion in dec in radians
double	px		#I the input parallax in arcseconds
double	rv		#I the input radial valocity in km / second
double	olng		#I the output ra in radians
double	olat		#I the output dec in radians

int	pmflag
double	pmr, pmd
double	sl_epb(), sl_epj()

begin
	# Check to see whether or not conversion / precession is necessary.
	if ((SKY_RADECSYS(cooin) == SKY_RADECSYS(cooout)) &&
	    (SKY_EQUINOX(cooin) == SKY_EQUINOX(cooout)) &&
	    (SKY_EPOCH(cooin) == SKY_EPOCH(cooout))) {
	    olng = ilng
	    olat = ilat
	    return
	}

	# Compute proper motions ?
	if (! IS_INDEFD(ipmlng) && ! IS_INDEFD(ipmlat))
	    pmflag = YES
	else
	    pmflag = NO

	switch (SKY_RADECSYS(cooin)) {

	# The input coordinate system is FK4 with or without the E terms.
	case EQTYPE_FK4, EQTYPE_FK4NOE:

	    if (pmflag == YES) {
		call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		    sl_epb (SKY_EPOCH(cooin)), sl_epb (SKY_EPOCH(cooout)),
		    olng, olat)
	    } else {
		olng = ilng
		olat = ilat
	    }
	    if (SKY_RADECSYS(cooin) == EQTYPE_FK4)
		call sl_suet (olng, olat, SKY_EQUINOX(cooin), olng, olat)
	    if (SKY_EQUINOX(cooin) != 1950.0d0)
	        call sl_prcs (1, SKY_EQUINOX(cooin), 1950.0d0, olng, olat) 
	    call sl_adet (olng, olat, 1950.0d0, olng, olat)
	    if (pmflag == YES)
	        call sl_f45z (olng, olat, sl_epb (SKY_EPOCH(cooout)),
		    olng, olat)
	    else
	        call sl_f45z (olng, olat, sl_epb (SKY_EPOCH(cooin)),
		    olng, olat)

	    switch (SKY_RADECSYS(cooout)) {

	    # The output coordinate system if FK4 with and without the E terms.
	    case EQTYPE_FK4, EQTYPE_FK4NOE:
		call sl_f54z (olng, olat, sl_epb (SKY_EPOCH(cooout)),
		    olng, olat, pmr, pmd)
	        call sl_suet (olng, olat, 1950.0d0, olng, olat)
		if (SKY_EQUINOX(cooout) != 1950.0d0)
	            call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout),
		        olng, olat) 
		if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
	            call sl_adet (olng, olat, SKY_EQUINOX(cooout), olng, olat)

	    # The output coordinate system if FK5.
	    case EQTYPE_FK5:
		if (SKY_EQUINOX(cooout) != 2000.0d0)
		    call sl_prcs (2, 2000.0d0, SKY_EQUINOX(cooout), olng, olat) 

	    # The output coordinate system if geocentric apparent.
	    case EQTYPE_GAPPT:
		call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.0d0,
		    SKY_EPOCH(cooout), olng, olat)
	    }

	# The input coordinate system is FK5 or geocentric apparent.
	case EQTYPE_FK5, EQTYPE_GAPPT:

	    if (SKY_RADECSYS(cooin) == EQTYPE_FK5) {
	        if (pmflag == YES) {
		    call sl_pm (ilng, ilat, ipmlng, ipmlat, px, rv,
		        sl_epj (SKY_EPOCH(cooin)), sl_epj (SKY_EPOCH(cooout)),
			    olng, olat)
	        } else {
	            olng = ilng
	            olat = ilat
		}
	    } else
	        call sl_amp (ilng, ilat, SKY_EPOCH(cooin), 2000.0d0, olng, olat)

	    switch (SKY_RADECSYS(cooout)) {

	    # The output coordinate system is FK4 with or without the E terms.
	    case EQTYPE_FK4, EQTYPE_FK4NOE:
	        if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_f54z (olng, olat, sl_epb(SKY_EPOCH(cooout)),
		    olng, olat, pmr, pmd)
		call sl_suet (olng, olat, 1950.0d0, olng, olat)
		if (SKY_EQUINOX(cooout) != 1950.0d0)
		    call sl_prcs (1, 1950.0d0, SKY_EQUINOX(cooout), olng, olat) 
		if (SKY_RADECSYS(cooout) == EQTYPE_FK4)
	            call sl_adet (olng, olat, SKY_EQUINOX(cooout), olng, olat)

	    # The output coordinate system is FK5.
	    case EQTYPE_FK5:
		if (SKY_EQUINOX(cooin) != SKY_EQUINOX(cooout))
		    call sl_prcs (2, SKY_EQUINOX(cooin), SKY_EQUINOX(cooout),
		        olng, olat) 

	    # The output coordinate system is geocentric apparent.
	    case EQTYPE_GAPPT:
	        if (SKY_EQUINOX(cooin) != 2000.0d0)
		    call sl_prcs (2, SKY_EQUINOX(cooin), 2000.0d0, olng, olat) 
		call sl_map (olng, olat, 0.0d0, 0.0d0, px, 0.0d0, 2000.0d0,
		    SKY_EPOCH(cooout), olng, olat)
	    }
	}
end
