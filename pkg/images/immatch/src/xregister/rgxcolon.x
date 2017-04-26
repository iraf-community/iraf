include <error.h>
include <imhdr.h>
include <imset.h>
include "xregister.h"

# RG_XCOLON-- Procedure to process colon commands for setting the cross-
# correlation parameters.

procedure rg_xcolon (gd, xc, imr, im1, im2, db, dformat, tfd, reglist, cmdstr,
	newdata, newcross, newcenter)

pointer	gd			#I pointer to the graphics stream
pointer	xc			#I pointer to cross-correlation structure
pointer	imr			#I/O pointer to the reference image
pointer	im1			#I/O pointer to the input image
pointer	im2			#I/O pointer to the output image
pointer	db			#I/O pointer to the shifts database file
int	dformat			#I is the shifts file in database format 
int	tfd			#I/O the transformations file descriptor
pointer	reglist			#I/O pointer to the regions list
char	cmdstr[ARB]		#I input command string
int	newdata			#I/O new input data
int	newcross		#I/O new cross-correlation function flag
int	newcenter		#I/O new cross-correlation peak flag

bool	streq()
int	ncmd, creg, nreg, ival, stat
pointer	sp, cmd, str
real	rval
int	strdic(), open(), nscan(), rg_xstati(), fntopnb()
int	rg_xregions(), rg_xmkregions(), strlen()
pointer	immap(), dtmap(), rg_xstatp()
real	rg_xstatr()
errchk	immap(), dtmap(), open(), fntopnb()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, XCMDS)
	switch (ncmd) {
	case XCMD_REFIMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_xstats (xc, REFIMAGE, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_REFIMAGE)
		    call pargstr (Memc[str])
	    } else {
		if (imr != NULL) {
		    call imunmap (imr)
		    imr = NULL
		}
		iferr {
		    imr = immap (Memc[cmd], READ_ONLY, 0)
		} then {
		    call erract (EA_WARN)
		    imr = immap (Memc[str], READ_ONLY, 0)
		} else if (IM_NDIM(imr) > 2 || IM_NDIM(imr) != IM_NDIM(im1)) {
		    call printf (
		    "Image has the wrong number of dimensions\n")
		    call imunmap (imr)
		    imr = immap (Memc[str], READ_ONLY, 0)
		} else {
		    call rg_xsets (xc, REFIMAGE, Memc[cmd])
		    newdata = YES; newcross = YES; newcenter = YES
		}
	    }

	case XCMD_IMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_xstats (xc, IMAGE, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_IMAGE)
		    call pargstr (Memc[str])
	    } else {
		if (im1 != NULL) {
		    call imunmap (im1)
		    im1 = NULL
		}
		iferr {
		    im1 = immap (Memc[cmd], READ_ONLY, 0)
		    call imseti (im1, IM_TYBNDRY, BT_NEAREST)
		    if (IM_NDIM(im1) == 1)
			call imseti (im1, IM_NBNDRYPIX, IM_LEN(im1,1))
		    else
		        call imseti (im1, IM_NBNDRYPIX,
			    max (IM_LEN(im1,1), IM_LEN(im1,2)))
		} then {
		    call erract (EA_WARN)
		    im1 = immap (Memc[str], READ_ONLY, 0)
		    call imseti (im1, IM_TYBNDRY, BT_NEAREST)
		    if (IM_NDIM(im1) == 1)
			call imseti (im1, IM_NBNDRYPIX, IM_LEN(im1,1))
		    else
		        call imseti (im1, IM_NBNDRYPIX,
			    max (IM_LEN(im1,1), IM_LEN(im1,2)))
		} else if (IM_NDIM(im1) > 2 || IM_NDIM(im1) != IM_NDIM(imr)) {
		    call printf (
		        "Image has the wrong number of dimensions\n")
		    call imunmap (im1)
		    im1 = immap (Memc[str], READ_ONLY, 0)
		    call imseti (im1, IM_TYBNDRY, BT_NEAREST)
		    if (IM_NDIM(im1) == 1)
			call imseti (im1, IM_NBNDRYPIX, IM_LEN(im1,1))
		    else
		        call imseti (im1, IM_NBNDRYPIX,
			    max (IM_LEN(im1,1), IM_LEN(im1,2)))
		} else {
		    call rg_xsets (xc, IMAGE, Memc[cmd])
		    newdata = YES; newcross = YES; newcenter = YES
		}
	    }

	case XCMD_OUTIMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_xstats (xc, OUTIMAGE, Memc[str], SZ_FNAME)
	    if (im2 == NULL || Memc[cmd] == EOS || streq (Memc[cmd],
	        Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_OUTIMAGE)
		    call pargstr (Memc[str])
	    } else {
		if (im2 != NULL) {
		    call imunmap (im2)
		    im2 = NULL
		}
		iferr {
		    im2 = immap (Memc[cmd], NEW_COPY, im1)
		} then {
		    call erract (EA_WARN)
		    im2 = immap (Memc[str], NEW_COPY, im1)
		} else {
		    call rg_xsets (xc, OUTIMAGE, Memc[cmd])
		}
	    }

	case XCMD_DATABASE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_xstats (xc, DATABASE, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s:  %s\n")
		    call pargstr (KY_DATABASE)
		    call pargstr (Memc[str])
	    } else {
		if (db != NULL) {
		    if (dformat == YES)
		        call dtunmap (db)
		    else
			call close (db)
		    db = NULL
		}
		iferr {
		    if (dformat == YES)
		        db = dtmap (Memc[cmd], APPEND)
		    else
			db = open (Memc[cmd], NEW_FILE, TEXT_FILE)
		} then {
		    call erract (EA_WARN)
		    if (dformat == YES)
		        db = dtmap (Memc[str], APPEND)
		    else
			db = open (Memc[str], APPEND, TEXT_FILE)
		} else {
		    call rg_xsets (xc, DATABASE, Memc[cmd])
		}
	    }

	CASE XCMD_RECORD:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call rg_xstats (xc, RECORD, Memc[str], SZ_FNAME)
		call printf ("%s: %s\n")
		    call pargstr (KY_RECORD)
		    call pargstr (Memc[str])
	    } else 
		call rg_xsets (xc, RECORD, Memc[cmd])

	case XCMD_CREGION:

	    call gargi (nreg)
	    creg = rg_xstati (xc, CREGION)

	    if (nscan() == 1 || (nreg == creg)) {
		call printf ("%s: %d/%d")
		    call pargstr (KY_CREGION)
		    call pargi (creg)
		    call pargi (rg_xstati (xc, NREGIONS))
		call printf ("  [%d:%d,%d:%d]\n")
		    call pargi (Memi[rg_xstatp (xc,RC1)+creg-1])
		    call pargi (Memi[rg_xstatp (xc,RC2)+creg-1])
		    call pargi (Memi[rg_xstatp (xc,RL1)+creg-1])
		    call pargi (Memi[rg_xstatp (xc,RL2)+creg-1])

	    } else {
		if (nreg < 1 || nreg > rg_xstati (xc,NREGIONS)) {
		    call printf ("Region %d is out of range\n")
			call pargi (nreg)
		} else {
		    call printf (
		        "Setting current region to %d: [%d:%d,%d:%d]\n")
			call pargi (nreg)
			call pargi (Memi[rg_xstatp (xc,RC1)+nreg-1])
			call pargi (Memi[rg_xstatp (xc,RC2)+nreg-1])
			call pargi (Memi[rg_xstatp (xc,RL1)+nreg-1])
			call pargi (Memi[rg_xstatp (xc,RL2)+nreg-1])
		    call rg_xseti (xc, CREGION, nreg)
		    newdata = YES; newcross = YES; newcenter = YES
		}

	    }

	case XCMD_REGIONS:

	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_xstats (xc, REGIONS, Memc[str], SZ_FNAME)
	    if (nscan() == 1 || streq (Memc[cmd], Memc[str]) || Memc[cmd] ==
	        EOS) {
		call printf ("%s [string/file]: %s\n")
		    call pargstr (KY_REGIONS)
		    call pargstr (Memc[str])
	    } else {
		if (reglist != NULL) {
		    call fntclsb (reglist)
		    reglist = NULL
		}
		iferr (reglist = fntopnb (Memc[cmd], NO))
		    reglist = NULL
		if (rg_xregions (reglist, imr, xc, 1) > 0) {
		    call rg_xseti (xc, CREGION, 1)
		    call rg_xsets (xc, REGIONS, Memc[cmd])
		    newdata = YES; newcross = YES; newcenter = YES
		} else {
		    if (reglist != NULL) {
		        call fntclsb (reglist)
		        reglist = NULL
		    }
		    iferr (reglist = fntopnb (Memc[str], NO))
		        reglist = NULL
		    if (rg_xregions (reglist, imr, xc, 1) > 0)
			;
		    call rg_xsets (xc, REGIONS, Memc[str])
		    call rg_xseti (xc, CREGION, 1)
		}
	    }

	case XCMD_REFFILE:

	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_xstats (xc, REFFILE, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s:  %s\n")
		    call pargstr (KY_REFFILE)
		    call pargstr (Memc[str])
	    } else {
		if (tfd != NULL) {
		    call close (tfd)
		    tfd = NULL
		}
		iferr {
		    tfd = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    tfd = NULL
		    call erract (EA_WARN)
		    call rg_xsets (xc, REFFILE, "")
		    call printf ("Coords file is undefined.\n")
		} else
		    call rg_xsets (xc, REFFILE, Memc[cmd])
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_XLAG:
	    call gargi (ival)
	    if (nscan () ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_XLAG)
		    call pargi (rg_xstati (xc, XLAG))
	    } else {
		call rg_xseti (xc, XLAG, ival)
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_YLAG:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_YLAG)
		    call pargi (rg_xstati (xc, YLAG))
	    } else {
		call rg_xseti (xc, YLAG, ival)
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_DXLAG:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_DXLAG)
		    call pargi (rg_xstati (xc, DXLAG))
	    } else {
		call rg_xseti (xc, DXLAG, ival)
	    }

	case XCMD_DYLAG:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_DYLAG)
		    call pargi (rg_xstati (xc, DYLAG))
	    } else {
		call rg_xseti (xc, DYLAG, ival)
	    }

	case XCMD_BACKGROUND:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] != EOS)
	        call strcat (" ", Memc[cmd], SZ_LINE)
	    call gargwrd (Memc[cmd+strlen(Memc[cmd])], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call rg_xstats (xc, BSTRING, Memc[str], SZ_FNAME)
		call printf ("%s:  %s\n")
		    call pargstr (KY_BACKGROUND)
		    call pargstr (Memc[str])
	    } else {
		call rg_xsets (xc, BSTRING, Memc[cmd])
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_BORDER:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_BORDER)
		    call pargi (rg_xstati (xc, BORDER))
	    } else {
		call rg_xseti (xc, BORDER, ival)
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_LOREJECT:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_LOREJECT)
		    call pargr (rg_xstatr (xc, LOREJECT))
	    } else {
		call rg_xsetr (xc, LOREJECT, rval)
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_HIREJECT:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_HIREJECT)
		    call pargr (rg_xstatr (xc, HIREJECT))
	    } else {
		call rg_xsetr (xc, HIREJECT, rval)
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_APODIZE:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_APODIZE)
		    call pargr (rg_xstatr (xc, APODIZE))
	    } else {
		call rg_xsetr (xc, APODIZE, max (0.0, min (rval, 0.50)))
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_CORRELATION:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call rg_xstats (xc, CSTRING, Memc[str], SZ_FNAME)
		call printf ("%s = %s\n")
		    call pargstr (KY_CORRELATION)
		    call pargstr (Memc[str])
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, XC_CTYPES)
		if (stat > 0) {
		    call rg_xseti (xc, CFUNC, stat)
		    call rg_xsets (xc, CSTRING, Memc[cmd])
		    newcross = YES; newcenter = YES
		}
	    }

	case XCMD_XWINDOW:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_XWINDOW)
		    call pargi (rg_xstati (xc, XWINDOW))
	    } else {
		call rg_xseti (xc, XWINDOW, ival)
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_YWINDOW:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_YWINDOW)
		    call pargi (rg_xstati (xc, YWINDOW))
	    } else {
		call rg_xseti (xc, YWINDOW, ival)
		newdata = YES; newcross = YES; newcenter = YES
	    }

	case XCMD_PEAKCENTER:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call rg_xstats (xc, PSTRING, Memc[str], SZ_FNAME)
		call printf ("%s: %s\n")
		    call pargstr (KY_PEAKCENTER)
		    call pargstr (Memc[str])
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, XC_PTYPES)
		if (stat > 0) {
		    call rg_xseti (xc, PFUNC, stat)
		    call rg_xsets (xc, PSTRING, Memc[cmd])
		    newcenter = YES
		}
	    }

	case XCMD_XCBOX:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_XCBOX)
		    call pargi (rg_xstati (xc, XCBOX))
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		call rg_xseti (xc, XCBOX, ival)
		newcenter = YES
	    }

	case XCMD_YCBOX:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_YCBOX)
		    call pargi (rg_xstati (xc, YCBOX))
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		call rg_xseti (xc, YCBOX, ival)
		newcenter = YES
	    }

	case XCMD_SHOW:
	    call gdeactivate (gd, 0)
	    call gargwrd (Memc[cmd], SZ_LINE)
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, XSHOW)
	    switch (ncmd) {
	    case XSHOW_DATA:
		call rg_xnshow (xc)
	    case XSHOW_BACKGROUND:
		call rg_xbshow (xc)
	    case XSHOW_CORRELATION:
		call rg_xxshow (xc)
	    case XSHOW_PEAKCENTER:
		call rg_xpshow (xc)
	    default:
		call rg_xshow (xc)
	    }
	    call greactivate (gd, 0)

	case XCMD_MARK:
	    call gdeactivate (gd, 0)
	    if (reglist != NULL) {
		call fntclsb (reglist)
		reglist = NULL
	    }
	    if (rg_xmkregions (imr, xc, 1, MAX_NREGIONS, Memc[str],
		SZ_LINE) <= 0) {
		call rg_xstats (xc, REGIONS, Memc[str], SZ_LINE)
		iferr (reglist = fntopnb (Memc[str], NO))
		    reglist = NULL
		if (rg_xregions (reglist, imr, xc, 1) > 0)
		    ;
		call rg_xsets (xc, REGIONS, Memc[str])
		call rg_xseti (xc, CREGION, 1)
	    } else {
		call rg_xseti (xc, CREGION, 1)
		call rg_xsets (xc, REGIONS, Memc[str])
	        newdata = YES; newcross = YES; newcenter = YES
	    }
	    call greactivate (gd, 0)
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
