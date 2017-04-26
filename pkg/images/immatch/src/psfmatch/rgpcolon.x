include <imhdr.h>
include <imset.h>
include <error.h>
include "psfmatch.h"

# RG_PCOLON -- Show/set the psfmatch task algorithm parameters. 

procedure rg_pcolon (gd, pm, imr, reglist, impsf, im1, imk, imfourier, im2,
	cmdstr, newref, newdata, newfourier, newfilter)

pointer	gd			#I pointer to the graphics stream
pointer	pm			#I pointer to psfmatch structure
pointer	imr			#I pointer to the reference image
int	reglist			#I the regions / psf list descriptor
pointer	impsf			#I pointer to the regions list
pointer	im1			#I pointer to the input image
pointer	imk			#I pointer to kernel image
pointer	imfourier		#I pointer to fourier spectrum image
pointer	im2			#I pointer to the output image
char	cmdstr[ARB]		#I command string
int	newref			#I/O new reference image
int	newdata			#I/O new input image
int	newfourier		#I/O new FFT
int	newfilter		#I/O new filter

bool	bval
int	ncmd, ival, stat, fd, ip
pointer	sp, cmd, str
real	rval
bool	itob()
bool	streq()
int	strdic(), nscan(), rg_pstati(), btoi(), rg_pregions(), fntopnb()
int	access(), rg_pmkregions(), open(), ctor()
pointer	immap()
real	rg_pstatr()
errchk	immap(), fntopnb()

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
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PMCMDS)
	switch (ncmd) {
	case PMCMD_REFIMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_pstats (pm, REFIMAGE, Memc[str], SZ_FNAME)
	    if (imr == NULL || Memc[cmd] == EOS || streq (Memc[cmd],
	        Memc[str])) {
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
                    "Reference image has the wrong number of dimensions\n")
                    call imunmap (imr)
                    imr = immap (Memc[str], READ_ONLY, 0)
		} else {
		    call rg_psets (pm, REFIMAGE, Memc[cmd])
		    newref = YES; newdata = YES
		    newfourier = YES; newfilter = YES
		}
	    }

	case PMCMD_IMAGE:

	    call gargwrd (Memc[cmd], SZ_LINE)
            call rg_pstats (pm, IMAGE, Memc[str], SZ_FNAME)
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
                    "Reference image has the wrong number of dimensions\n")
                    call imunmap (im1)
                    im1 = immap (Memc[str], READ_ONLY, 0)
                    call imseti (im1, IM_TYBNDRY, BT_NEAREST)
                    if (IM_NDIM(im1) == 1)
                        call imseti (im1, IM_NBNDRYPIX, IM_LEN(im1,1))
                    else
                        call imseti (im1, IM_NBNDRYPIX,
                            max (IM_LEN(im1,1), IM_LEN(im1,2)))
		} else {
                    call rg_psets (pm, IMAGE, Memc[cmd])
                    newdata = YES; newref = YES
		    newfourier = YES; newfilter = YES
                }
            }

	case PMCMD_PSFDATA:

	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_pstats (pm, PSFDATA, Memc[str], SZ_FNAME)
	    if (reglist == NULL || nscan() == 1 || (streq (Memc[cmd],
	        Memc[str]) && Memc[cmd] != EOS)) {
		call printf ("%s [string/file]:  %s\n")
		    call pargstr (KY_PSFDATA)
		    call pargstr (Memc[str])
	    } else if (rg_pstati(pm, CONVOLUTION) == PM_CONIMAGE) {
		call fntclsb (reglist)
		iferr {
		    reglist = fntopnb (Memc[cmd], NO)
		} then {
		    reglist = fntopnb (Memc[str], NO)
		} else {
		    if (rg_pregions (reglist, imr, pm, 1, NO) > 0)
			;
		    call rg_psets (pm, PSFDATA, Memc[cmd])
		    newdata = YES; newref = YES
		    newfourier = YES; newfilter = YES
		}
	    }

	case PMCMD_PSFIMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_pstats (pm, PSFIMAGE, Memc[str], SZ_FNAME)
	    if (impsf == NULL || Memc[cmd] == EOS || streq (Memc[cmd],
	        Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_PSFIMAGE)
		    call pargstr (Memc[str])
	    } else {
		if (impsf != NULL) {
		    call imunmap (impsf)
		    impsf = NULL
		}
		iferr {
		    impsf = immap (Memc[cmd], READ_ONLY, 0)
		} then {
		    call erract (EA_WARN)
		    impsf = immap (Memc[str], READ_ONLY, 0)
		} else if (IM_NDIM(impsf) > 2 || IM_NDIM(impsf) !=
		    IM_NDIM(imr)) {
		    call printf (
                    "PSF image has the wrong number of dimensions\n")
                    call imunmap (impsf)
                    impsf = immap (Memc[str], READ_ONLY, 0)
		} else {
		    call rg_psets (pm, PSFIMAGE, Memc[cmd])
		    newref = YES; newdata = YES
		    newfourier = YES; newfilter = YES
		}
	    }

	case PMCMD_KERNEL:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_pstats (pm, KERNEL, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_KERNEL)
		    call pargstr (Memc[str])
	    } else {
		if (imk != NULL) {
		    call imunmap (imk)
		    call imdelete (Memc[str])
		    imk = NULL
		}
		iferr {
		    imk = immap (Memc[cmd], NEW_IMAGE, 0)
		} then {
		    call erract (EA_WARN)
		    imk = NULL
		    call rg_psets (pm, KERNEL, "")
		} else
		    call rg_psets (pm, KERNEL, Memc[cmd])
	    }


	case PMCMD_OUTIMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_pstats (pm, OUTIMAGE, Memc[str], SZ_FNAME)
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
		    call rg_psets (pm, OUTIMAGE, Memc[cmd])
		}
	    }

	case PMCMD_DNX:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_DNX)
		    call pargi (rg_pstati (pm, DNX))
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		call rg_pseti (pm, DNX, ival)
		newref = YES; newdata = YES; newfourier = YES; newfilter = YES
	    }

	case PMCMD_DNY:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_DNY)
		    call pargi (rg_pstati (pm, DNY))
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		call rg_pseti (pm, DNY, ival)
		newref = YES; newdata = YES; newfourier = YES; newfilter = YES
	    }

	case PMCMD_PNX:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_PNX)
		    call pargi (rg_pstati (pm, PNX))
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		call rg_pseti (pm, PNX, min (ival, rg_pstati (pm, DNX)))
		newref = YES; newdata = YES; newfourier = YES; newfilter = YES
	    }

	case PMCMD_PNY:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_PNY)
		    call pargi (rg_pstati (pm, PNY))
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		call rg_pseti (pm, PNY, min (ival, rg_pstati(pm, DNY)))
		newref = YES; newdata = YES; newfourier = YES; newfilter = YES
	    }

	case PMCMD_CENTER:
	    call gargb (bval)
	    if (nscan() ==  1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_CENTER)
		    call pargb (itob (rg_pstati (pm, CENTER)))
	    } else {
		call rg_pseti (pm, CENTER, btoi (bval))
		newfourier = YES; newfilter = YES
	    }

	case PMCMD_BACKGRD:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call rg_pstats (pm, BSTRING, Memc[str], SZ_FNAME)
		call printf ("%s:  %s\n")
		    call pargstr (KY_BACKGRD)
		    call pargstr (Memc[str])
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PM_BTYPES)
		ip = 1
		if (stat > 0) {
		    call rg_pseti (pm, BACKGRD, stat)
		    call rg_psets (pm, BSTRING, Memc[cmd])
		    newfourier = YES; newfilter = YES
		} else if (ctor (str, ip, rval) > 0) {
                    call rg_psetr (pm, BVALUE, rval)
                    if (ctor (str, ip, rval) > 0) {
                        call rg_psetr (pm, BVALUER, rval)
                        call strcpy (str, PM_BSTRING(pm), SZ_FNAME)
                        call rg_pseti (pm, BACKGRD, PM_NUMBER)
                    } else {
                        call rg_psetr (pm, BVALUE, 0.0)
                        call rg_psetr (pm, BVALUER, 0.0)
                    }
                }
	    }

	case PMCMD_LOREJECT:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_LOREJECT)
		    call pargr (rg_pstatr (pm, LOREJECT))
	    } else {
		call rg_psetr (pm, LOREJECT, rval)
		newfourier = YES; newfilter = YES
	    }

	case PMCMD_HIREJECT:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_HIREJECT)
		    call pargr (rg_pstatr (pm, HIREJECT))
	    } else {
		call rg_psetr (pm, HIREJECT, rval)
		newfourier = YES; newfilter = YES
	    }

	case PMCMD_APODIZE:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_APODIZE)
		    call pargr (rg_pstatr (pm, APODIZE))
	    } else {
		call rg_psetr (pm, APODIZE, rval)
		newfourier = YES; newfilter = YES
	    }

	case PMCMD_CONVOLUTION:
	    if (Memc[cmd] == EOS) {
		call rg_pstats (pm, CSTRING, Memc[str], SZ_LINE)
		call printf ("%s: %s\n")
		    call pargstr (KY_CONVOLUTION)
		    call pargstr (Memc[str])
	    }

	case PMCMD_UFLUXRATIO:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_UFLUXRATIO)
		    call pargr (rg_pstatr (pm, UFLUXRATIO))
	    } else {
		call rg_psetr (pm, UFLUXRATIO, rval)
		newfourier = YES; newfilter = YES
	    }

	case PMCMD_FILTER:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call rg_pstats (pm, FSTRING, Memc[str], SZ_LINE)
		call printf ("%s: %s\n")
		    call pargstr (KY_FILTER)
		    call pargstr (Memc[str])
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PM_FTYPES)
		if (stat > 0) {
		    call rg_pseti (pm, FILTER, stat)
		    call rg_psets (pm, FSTRING, Memc[cmd])
		}
		newfilter = YES
	    }

	case PMCMD_SXINNER:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_SXINNER)
		    call pargr (rg_pstatr (pm, SXINNER))
	    } else {
		call rg_psetr (pm, SXINNER, rval)
		newfilter = YES
	    }

	case PMCMD_SXOUTER:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_SXOUTER)
		    call pargr (rg_pstatr (pm, SXOUTER))
	    } else {
		call rg_psetr (pm, SXOUTER, rval)
		newfilter = YES
	    }

	case PMCMD_SYINNER:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_SYINNER)
		    call pargr (rg_pstatr (pm, SYINNER))
	    } else {
		call rg_psetr (pm, SYINNER, rval)
		newfilter = YES
	    }

	case PMCMD_SYOUTER:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_SYOUTER)
		    call pargr (rg_pstatr (pm, SYOUTER))
	    } else {
		call rg_psetr (pm, SYOUTER, rval)
		newfilter = YES
	    }

	case PMCMD_RADSYM:
	    call gargb (bval)
	    if (nscan() ==  1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_RADSYM)
		    call pargb (itob (rg_pstati (pm, RADSYM)))
	    } else {
		call rg_pseti (pm, RADSYM, btoi (bval))
		newfilter = YES
	    }

	case PMCMD_THRESHOLD:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_THRESHOLD)
		    call pargr (rg_pstatr (pm, THRESHOLD))
	    } else {
		call rg_psetr (pm, THRESHOLD, rval)
		newfilter = YES
	    }

	case PMCMD_NORMFACTOR:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_NORMFACTOR)
		    call pargr (rg_pstatr (pm, NORMFACTOR))
	    } else {
		call rg_psetr (pm, NORMFACTOR, rval)
		newfilter = YES
	    }

	case PMCMD_SHOW:
	    call gdeactivate (gd, 0)
	    call rg_pshow (pm)
	    call greactivate (gd, 0)

	case PMCMD_MARK:
	    call gdeactivate (gd, 0)
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		fd = NULL
	    } else if (access (Memc[cmd], 0, 0) == YES) {
		call printf ("Warning: file %s already exists\n")
		    call pargstr (Memc[cmd])
		fd = NULL
	    } else {
	        fd = open (Memc[cmd], NEW_FILE, TEXT_FILE)
	    }
	    call printf ("\n")
	    if (rg_pmkregions (fd, imr, pm, 1, MAX_NREGIONS) <= 0)
		call printf ("The regions list is empty\n")
	    newdata = YES; newref = YES
	    newfourier = YES; newfilter = YES
	    call printf ("\n")
	    if (fd != NULL)
	        call close (fd)
	    call greactivate (gd, 0)

	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
