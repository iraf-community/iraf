include <imhdr.h>
include <error.h>
include "linmatch.h"

# RG_LCOLON -- Show/set the linmatch task algorithm parameters. 

procedure rg_lcolon (gd, ls, imr, im1, im2, db, dformat, reglist, rpfd, ipfd,
	sfd, cmdstr, newref, newimage, newfit, newavg)

pointer	gd			#I pointer to the graphics stream
pointer ls			#I pointer to linmatch structure
pointer	imr			#I pointer to the reference image
pointer	im1			#I pointer to the input image
pointer	im2			#I pointer to the output image
pointer	db			#I pointer to the databas file
int	dformat			#I the database file format
int	reglist			#I the regions / photometry file descriptor
int	rpfd			#I the reference photometry file descriptor
int	ipfd			#I the input photometry file descriptor
int	sfd			#I the shifts file descriptor
char	cmdstr[ARB]		#I command string
int	newref			#I/O new reference image
int	newimage		#I/O new input image
int	newfit			#I/O new fit
int	newavg			#I/O new averages

int	ncmd, nref, nim, ival, fd
pointer	sp, cmd, str
real	rval
bool	streq()
int	strdic(), rg_lstati(), rg_lregions(), open(), fntopnb(), nscan()
int	rg_lrphot(), access(), rg_lmkxy(), rg_lmkregions()
pointer	immap(), dtmap()
real	rg_lstatr()
errchk	immap(), open(), fntopnb()

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
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, LSCMDS)

	switch (ncmd) {

	case LSCMD_REFIMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_lstats (ls, REFIMAGE, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_REFIMAGE)
		    call pargstr (Memc[str])
	    } else if (rg_lstati(ls, BSALGORITHM) == LS_PHOTOMETRY ||
	        rg_lstati(ls, BZALGORITHM) == LS_PHOTOMETRY) {
		if (rpfd != NULL) {
		    call close (rpfd)
		    rpfd = NULL
		}
		iferr {
		    rpfd = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    call erract (EA_WARN)
		    rpfd = open (Memc[str], READ_ONLY, TEXT_FILE)
		    if (rg_lrphot (rpfd, ls, 1, rg_lstati(ls, MAXNREGIONS),
		        YES) <= 0)
			;
		    call seek (ipfd, BOF)
		    if (rg_lrphot (ipfd, ls, 1, rg_lstati(ls, NREGIONS),
		        NO) <= 0)
			;
		} else {
		    nref = rg_lrphot (rpfd, ls, 1, rg_lstati(ls, MAXNREGIONS),
		        YES)
		    if (nref > 0) {
		        call seek (ipfd, BOF)
		        nim = rg_lrphot (ipfd, ls, 1, rg_lstati(ls, NREGIONS),
			    NO)
			if (nim < nref)
			    call printf ("There are too few input points\n")
		    } else {
			call close (rpfd)
		        rpfd = open (Memc[str], READ_ONLY, TEXT_FILE)
		        if (rg_lrphot (rpfd, ls, 1, rg_lstati(ls, MAXNREGIONS),
		            YES) <= 0)
			    ;
		        call seek (ipfd, BOF)
		        if (rg_lrphot (ipfd, ls, 1, rg_lstati(ls, NREGIONS),
		            NO) <= 0)
			    ;
			call printf (
			    "The new reference photometry file is empty\n")
		    }
		    call rg_lsets (ls, REFIMAGE, Memc[cmd])
		    newref = YES; newimage = YES; newfit = YES; newavg = YES
		}
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
		    call rg_lgain (imr, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,GAIN)))
                        call rg_lsetr (ls, RGAIN, rg_lstatr (ls,GAIN))
                    call rg_lrdnoise (imr, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,READNOISE)))
                        call rg_lsetr (ls, RREADNOISE, rg_lstatr (ls,READNOISE))
		    call rg_lsets (ls, REFIMAGE, Memc[cmd])
		    newref = YES; newimage = YES; newfit = YES; newavg = YES
		}
	    }

	case LSCMD_IMAGE:

	    call gargwrd (Memc[cmd], SZ_LINE)
            call rg_lstats (ls, IMAGE, Memc[str], SZ_FNAME)
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
                } then {
                    call erract (EA_WARN)
                    im1 = immap (Memc[str], READ_ONLY, 0)
                } else if (IM_NDIM(im1) > 2 || IM_NDIM(im1) != IM_NDIM(imr)) {
		    call printf (
                    "Reference image has the wrong number of dimensions\n")
                    call imunmap (im1)
                    im1 = immap (Memc[str], READ_ONLY, 0)
		} else {
		    call rg_lgain (im1, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,GAIN)))
                        call rg_lsetr (ls, IGAIN, rg_lstatr (ls,GAIN))
                    call rg_lrdnoise (im1, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,READNOISE)))
                        call rg_lsetr (ls, IREADNOISE, rg_lstatr (ls,READNOISE))
                    call rg_lsets (ls, IMAGE, Memc[cmd])
                    newimage = YES; newref = YES; newfit = YES; newavg = YES
                }
            }

	case LSCMD_REGIONS:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_lstats (ls, REGIONS, Memc[str], SZ_FNAME)
	    if (reglist == NULL || nscan() == 1 || (streq (Memc[cmd],
	        Memc[str]) && Memc[cmd] != EOS)) {
		call printf ("%s [string/file]:  %s\n")
		    call pargstr (KY_REGIONS)
		    call pargstr (Memc[str])
	    } else if (rg_lstati(ls, BSALGORITHM) != LS_PHOTOMETRY &&
	        rg_lstati(ls, BZALGORITHM) != LS_PHOTOMETRY) {
		call fntclsb (reglist)
		iferr {
		    reglist = fntopnb (Memc[cmd], NO)
		} then {
		    reglist = fntopnb (Memc[str], NO)
		} else {
		    if (rg_lregions (reglist, imr, ls, 1, NO) > 0)
			;
		    call rg_lsets (ls, REGIONS, Memc[cmd])
		    newimage = YES; newref = YES; newfit = YES; newavg = YES
		}
	    }

	case LSCMD_PHOTFILE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_lstats (ls, PHOTFILE, Memc[str], SZ_FNAME)
	    if (ipfd == NULL || Memc[cmd] == EOS || streq (Memc[cmd],
	        Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_PHOTFILE)
		    call pargstr (Memc[str])
	    } else {
		if (ipfd != NULL) {
		    call close (ipfd)
		    ipfd = NULL
		}
		iferr {
		    ipfd = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    call erract (EA_WARN)
		    ipfd = open (Memc[str], READ_ONLY, TEXT_FILE)
		} else {
		    nim = rg_lrphot (ipfd, ls, 1, rg_lstati(ls, NREGIONS),
		        NO)
		    if (nim > 0) {
		        call rg_lsets (ls, PHOTFILE, Memc[cmd])
		        newref = YES; newimage = YES
			newfit = YES; newavg = YES
		    } else {
			call close (ipfd)
		        ipfd = open (Memc[str], READ_ONLY, TEXT_FILE)
		        nim = rg_lrphot (ipfd, ls, 1, rg_lstati(ls, NREGIONS),
		            NO)
		    }
		}
	    }

	case LSCMD_SHIFTSFILE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_lstats (ls, SHIFTSFILE, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd],
	        Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_SHIFTSFILE)
		    call pargstr (Memc[str])
	    } else {
		if (sfd != NULL) {
		    call close (sfd)
		    sfd = NULL
		}
		iferr {
		    sfd = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    call erract (EA_WARN)
		    sfd = open (Memc[str], READ_ONLY, sfd)
		} else {
                    call rg_lgshift (sfd, ls)
		    call rg_lstats (ls, SHIFTSFILE, Memc[cmd], SZ_FNAME)
		}
	    }

	case LSCMD_OUTIMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call rg_lstats (ls, OUTIMAGE, Memc[str], SZ_FNAME)
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
		    call rg_lsets (ls, OUTIMAGE, Memc[cmd])
		}
	    }

	case LSCMD_DATABASE:
            call gargwrd (Memc[cmd], SZ_LINE)
            call rg_lstats (ls, DATABASE, Memc[str], SZ_FNAME)
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
                    call rg_lsets (ls, DATABASE, Memc[cmd])
                }
            }

        CASE LSCMD_RECORD:
            call gargstr (Memc[cmd], SZ_LINE)
            if (Memc[cmd] == EOS) {
                call rg_lstats (ls, RECORD, Memc[str], SZ_FNAME)
                call printf ("%s: %s\n")
                    call pargstr (KY_RECORD)
                    call pargstr (Memc[str])
            } else
                call rg_lsets (ls, RECORD, Memc[cmd])

	case LSCMD_XSHIFT:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_XSHIFT)
		    call pargr (rg_lstatr (ls, XSHIFT))
	    } else {
		call rg_lsetr (ls, XSHIFT, rval)
		if (sfd == NULL) {
		    call rg_lsetr (ls, SXSHIFT, rg_lstatr (ls, XSHIFT))
		    call rg_lsetr (ls, SYSHIFT, rg_lstatr (ls, YSHIFT))
		}
		newref = YES; newimage = YES; newfit = YES; newavg = YES
	    }

	case LSCMD_YSHIFT:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_YSHIFT)
		    call pargr (rg_lstatr (ls, YSHIFT))
	    } else {
		call rg_lsetr (ls, YSHIFT, rval)
		if (sfd == NULL) {
		    call rg_lsetr (ls, SXSHIFT, rg_lstatr (ls, XSHIFT))
		    call rg_lsetr (ls, SYSHIFT, rg_lstatr (ls, YSHIFT))
		}
		newref = YES; newimage = YES; newfit = YES; newavg = YES
	    }

	case LSCMD_DNX:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_DNX)
		    call pargi (rg_lstati (ls, DNX))
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		call rg_lseti (ls, DNX, ival)
		newref = YES; newimage = YES; newfit = YES; newavg = YES
	    }

	case LSCMD_DNY:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_DNY)
		    call pargi (rg_lstati (ls, DNY))
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		call rg_lseti (ls, DNY, ival)
		newref = YES; newimage = YES; newfit = YES; newavg = YES
	    }

	case LSCMD_MAXNREGIONS:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_MAXNREGIONS)
		    call pargi (rg_lstati (ls, MAXNREGIONS))
	    }

	case LSCMD_DATAMIN:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_DATAMIN)
		    call pargr (rg_lstatr (ls, DATAMIN))
	    } else {
		call rg_lsetr (ls, DATAMIN, rval)
		if (rg_lstati(ls,BSALGORITHM) != LS_PHOTOMETRY &&
		    rg_lstati(ls,BZALGORITHM) != LS_PHOTOMETRY)
		    newfit = YES; newavg = YES
	    }

	case LSCMD_DATAMAX:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_DATAMAX)
		    call pargr (rg_lstatr (ls, DATAMAX))
	    } else {
		call rg_lsetr (ls, DATAMAX, rval)
		if (rg_lstati(ls,BSALGORITHM) != LS_PHOTOMETRY &&
		    rg_lstati(ls,BZALGORITHM) != LS_PHOTOMETRY)
		    newfit = YES; newavg = YES
	    }

	case LSCMD_MAXITER:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_MAXITER)
		    call pargi (rg_lstati (ls, MAXITER))
	    } else {
		call rg_lseti (ls, MAXITER, ival)
		if (rg_lstati(ls,BSALGORITHM) != LS_PHOTOMETRY &&
		    rg_lstati(ls,BZALGORITHM) != LS_PHOTOMETRY) {
		    if (rg_lstati(ls,BSALGORITHM) == LS_FIT &&
		        rg_lstati(ls,BZALGORITHM) == LS_FIT) {
		        newfit = YES; newavg = YES
		    } else
			newavg = YES
		}
	    }

	case LSCMD_NREJECT:
	    call gargi (ival)
	    if (nscan() ==  1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_NREJECT)
		    call pargi (rg_lstati (ls, NREJECT))
	    } else {
		call rg_lseti (ls, NREJECT, ival)
		newfit = YES; newavg = YES
		if (rg_lstati(ls,BSALGORITHM) == LS_FIT ||
		    rg_lstati(ls,BZALGORITHM) == LS_FIT) 
		    newfit = YES
		newavg = YES
	    }

	case LSCMD_LOREJECT:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_LOREJECT)
		    call pargr (rg_lstatr (ls, LOREJECT))
	    } else {
		call rg_lsetr (ls, LOREJECT, rval)
		if (rg_lstati(ls,BSALGORITHM) == LS_FIT ||
		    rg_lstati(ls,BZALGORITHM) == LS_FIT) 
		    newfit = YES
		newavg = YES
	    }

	case LSCMD_HIREJECT:
	    call gargr (rval)
	    if (nscan() ==  1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_HIREJECT)
		    call pargr (rg_lstatr (ls, HIREJECT))
	    } else {
		call rg_lsetr (ls, HIREJECT, rval)
		if (rg_lstati(ls,BSALGORITHM) == LS_FIT ||
		    rg_lstati(ls,BZALGORITHM) == LS_FIT) 
		    newfit = YES
		newavg = YES
	    }

	case LSCMD_GAIN:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call rg_lstats (ls, CCDGAIN, Memc[str], SZ_LINE)
		call printf ("%s: %s\n")
		    call pargstr (KY_GAIN)
		    call pargstr (Memc[str])
	    } else {
		call rg_lsets (ls, CCDGAIN, Memc[cmd])
		if (imr != NULL) {
		    call rg_lgain (imr, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,GAIN)))
		        call rg_lsetr (ls, RGAIN, rg_lstatr(ls,GAIN)) 
		}
		if (im1 != NULL) {
		    call rg_lgain (im1, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,GAIN)))
		        call rg_lsetr (ls, IGAIN, rg_lstatr(ls,GAIN)) 
		}
		newfit = YES; newavg = YES
	    }

	case LSCMD_READNOISE:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call rg_lstats (ls, CCDREAD, Memc[str], SZ_LINE)
		call printf ("%s: %s\n")
		    call pargstr (KY_READNOISE)
		    call pargstr (Memc[str])
	    } else {
		call rg_lsets (ls, CCDREAD, Memc[cmd])
		if (imr != NULL) {
		    call rg_lrdnoise (imr, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,READNOISE)))
		        call rg_lsetr (ls, RREADNOISE, rg_lstatr(ls,READNOISE)) 
		}
		if (im1 != NULL) {
		    call rg_lrdnoise (im1, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,READNOISE)))
		        call rg_lsetr (ls, IREADNOISE, rg_lstatr(ls,READNOISE)) 
		}
		newfit = YES; newavg = YES
	    }

	case LSCMD_SHOW:
	    call gdeactivate (gd, 0)
	    call rg_lshow (ls)
	    call greactivate (gd, 0)

	case LSCMD_MARKCOORDS, LSCMD_MARKSECTIONS:
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
	    if (imr == NULL || im1 == NULL) {
		call printf ("The reference or input image is undefined.\n")
	    } else {
		if (reglist != NULL) {
		    call fntclsb (reglist)
		    reglist = NULL
		}
	        if (ncmd == LSCMD_MARKCOORDS) {
	            nref = rg_lmkxy (fd, imr, ls, 1, rg_lstati (ls,
	                MAXNREGIONS))
	        } else {
	            nref = rg_lmkregions (fd, imr, ls, 1, rg_lstati (ls,
	                MAXNREGIONS), Memc[str], SZ_LINE)
		}
		if (nref <= 0) {
		    call rg_lstats (ls, REGIONS, Memc[str], SZ_LINE)
                    iferr (reglist = fntopnb (Memc[str], NO))
                        reglist = NULL
                    if (rg_lregions (reglist, imr, ls, 1, 1) > 0)
                    	;
                    call rg_lsets (ls, REGIONS, Memc[str])
                    call rg_lseti (ls, CNREGION, 1)
		} else {
		    call rg_lseti (ls, CNREGION, 1)
		    call rg_lsets (ls, REGIONS, Memc[str])
		    newref = YES; newimage = YES
		    newfit = YES; newavg = YES
		}
	    }
	    call printf ("\n")
	    if (fd != NULL)
	        call close (fd)
	    call greactivate (gd, 0)

	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
