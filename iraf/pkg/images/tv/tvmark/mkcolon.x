include <imhdr.h>
include <error.h>
include <fset.h>
include "tvmark.h"

# MK_COLON -- Procedure to process immark colon commands.

procedure mk_colon (mk, cmdstr, im, iw, sim, log, cl, ltid, dl)

pointer	mk		# pointer to the immark structure
char	cmdstr[ARB]	# command string
pointer	im		# pointer to the frame buffer
pointer	iw		# pointer to the wcs information
pointer	sim		# pointer to a scratch image
int	log		# log file descriptor
int	cl		# coords file descriptor
int	ltid		# coords file sequence number
int	dl		# deletions file descriptor

bool	bval
real	rval
pointer	sp, cmd, str, outim, deletions, ext
int	ncmd, mark, font, ival, ip, nchars, wcs_status

real	mk_statr()
bool	itob(), streq()
pointer	immap(), imd_mapframe(), iw_open()
int	open(), strdic(), nscan(), mk_stati(), btoi(), ctowrd()
errchk	imd_mapframe(), iw_open(), immap(), imunmap(), open()

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (deletions, SZ_FNAME, TY_CHAR)
	call salloc (ext, SZ_FNAME, TY_CHAR)

	# Get the command.
	ip = 1
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MKCMDS)
	switch (ncmd) {
	case MKCMD_IMAGE:

	case MKCMD_OUTIMAGE:
	    call gargstr (Memc[cmd], SZ_LINE)
	    call mk_stats (mk, OUTIMAGE, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s:  %s\n")
		    call pargstr (KY_OUTIMAGE)
		    call pargstr (Memc[str])
	    } else {
		nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		call mk_sets (mk, OUTIMAGE, Memc[str])
	    }

	case MKCMD_DELETIONS:
	    call gargstr (Memc[cmd], SZ_LINE)
	    call mk_stats (mk, DELETIONS, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s:  %s\n")
		    call pargstr (KY_DELETIONS)
		    call pargstr (Memc[str])
	    } else {
		nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		call mk_sets (mk, DELETIONS, Memc[str])
	    }

	case MKCMD_SNAP:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call mk_stats (mk, OUTIMAGE, Memc[str], SZ_FNAME)
		if (Memc[str] == EOS)
		    call mk_stats (mk, IMAGE, Memc[str], SZ_FNAME)
		call mk_imname (Memc[str], "", "snap", Memc[cmd], SZ_FNAME)
	    }

	    iferr {
		outim = immap (Memc[cmd], NEW_COPY, im)
		call printf ("Creating image: %s - ")
		    call pargstr (Memc[cmd])
		call flush (STDOUT)
		call mk_imcopy (im, outim)
		call imunmap (outim)
	    } then {
		call printf ("\n")
		call erract (EA_WARN)
	    } else {
		call printf ("done\n")
	    }

	case MKCMD_COORDS:
	    call gargstr (Memc[cmd], SZ_LINE)
	    call mk_stats (mk, COORDS, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s:  %s\n")
		    call pargstr (KY_COORDS)
		    call pargstr (Memc[str])
	    } else {
		nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		if (cl != NULL) {
		    call close( cl)
		    call close (dl)
		    cl = NULL
		    dl = NULL
		}
		iferr {
		    if (Memc[str] != EOS) {
		        iferr (cl = open (Memc[str], READ_WRITE, TEXT_FILE)) {
			    cl = open (Memc[str], NEW_FILE, TEXT_FILE)
			    call close (cl)
			    cl = open (Memc[str], READ_WRITE, TEXT_FILE)
			    call mk_stats (mk, DELETIONS, Memc[ext], SZ_FNAME)
			    call sprintf (Memc[deletions], SZ_FNAME, "%s.%s")
				call pargstr (Memc[str])
			    if (Memc[ext] == EOS)
				call pargstr ("del")
			    else
				call pargstr (Memc[ext])
			}
		    }
		} then {
		    cl = NULL
		    dl = NULL
		    call erract (EA_WARN)
		    call mk_sets (mk, COORDS, "")
		} else {
		    call mk_sets (mk, COORDS, Memc[str])
		}
		ltid = 0
	    }

	case MKCMD_LOGFILE:
	    call gargstr (Memc[cmd], SZ_LINE)
	    call mk_stats (mk, LOGFILE, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s:  %s\n")
		    call pargstr (KY_LOGFILE)
		    call pargstr (Memc[str])
	    } else {
		nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		if (log != NULL) {
		    call close (log)
		    log = NULL
		}
		iferr {
		    if (Memc[str] != EOS)
		        log = open (Memc[str], NEW_FILE, TEXT_FILE)
		} then {
		    log = NULL
		    call erract (EA_WARN)
		    call mk_sets (mk, LOGFILE, "")
		    call printf ("Log file is undefined.\n")
		} else
		    call mk_sets (mk, LOGFILE, Memc[str])
	    }

	case MKCMD_AUTOLOG:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_AUTOLOG)
		    call pargb (itob (mk_stati (mk, AUTOLOG)))
	    } else
		call mk_seti (mk, AUTOLOG, btoi (bval))

	case MKCMD_FRAME:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_FRAME)
		    call pargi (mk_stati (mk, FRAME))
	    } else if (ival != mk_stati (mk, FRAME)) {
		call iw_close (iw)
		call imunmap (im)
		iferr {
		    im = imd_mapframe (ival, READ_WRITE, YES)
		    iw = iw_open (im, ival, Memc[str], SZ_FNAME, wcs_status)
		    call mk_sets (mk, IMAGE, Memc[str])
		} then {
		    call erract (EA_WARN)
		    im = imd_mapframe (mk_stati(mk,FRAME), READ_WRITE, YES)
		    iw = iw_open (im, mk_stati(mk,FRAME),
			Memc[str], SZ_FNAME, wcs_status)
		    call mk_sets (mk, IMAGE, Memc[str])
		} else 
		    call mk_seti (mk, FRAME, ival)
	    }

	case MKCMD_FONT:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
	        call mk_stats (mk, FONT, Memc[cmd], SZ_LINE)
	        call printf ("%s = %s\n")
		    call pargstr (KY_FONT)
		    call pargstr (Memc[cmd])
	    } else {
		font = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MKFONTLIST)
		if (font > 0)
		    call mk_sets (mk, FONT, Memc[cmd])
	    }

	case MKCMD_LABEL:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_LABEL)
		    call pargb (itob (mk_stati (mk, LABEL)))
	    } else
		call mk_seti (mk, LABEL, btoi (bval))

	case MKCMD_NUMBER:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_NUMBER)
		    call pargb (itob (mk_stati (mk, NUMBER)))
	    } else
		call mk_seti (mk, NUMBER, btoi (bval))

	case MKCMD_NXOFFSET:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_NXOFFSET)
		    call pargi (mk_stati (mk, NXOFFSET))
	    } else
		call mk_seti (mk, NXOFFSET, ival)

	case MKCMD_NYOFFSET:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_NYOFFSET)
		    call pargi (mk_stati (mk, NYOFFSET))
	    } else
		call mk_seti (mk, NYOFFSET, ival)

	case MKCMD_GRAYLEVEL:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_GRAYLEVEL)
		    call pargi (mk_stati (mk, GRAYLEVEL))
	    } else
		call mk_seti (mk, GRAYLEVEL, ival)

	case MKCMD_SZPOINT:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_SZPOINT)
		    call pargi (2 * mk_stati (mk, SZPOINT) + 1)
	    } else {
		if (mod (ival, 2) == 0)
		    ival = ival + 1
		ival = ival / 2
		call mk_seti (mk, SZPOINT, ival)
	    }

	case MKCMD_SIZE:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_SIZE)
		    call pargi (mk_stati (mk, SIZE))
	    } else
		call mk_seti (mk, SIZE, ival)

	case MKCMD_TOLERANCE:
	    call gargr (rval)
	    if (nscan () == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_TOLERANCE)
		    call pargr (mk_statr (mk, TOLERANCE))
	    } else
		call mk_setr (mk, TOLERANCE, rval)

	case MKCMD_MARK:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
	        call mk_stats (mk, MARK, Memc[cmd], SZ_LINE)
	        call printf ("%s = %s\n")
		    call pargstr (KY_MARK)
		    call pargstr (Memc[cmd])
	    } else {
		mark = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MKTYPELIST)
		if (mark > 0) {
		    call mk_seti (mk, MKTYPE, mark)
		    call mk_sets (mk, MARK, Memc[cmd])
		}
	    }

	case MKCMD_CIRCLES:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
	        call mk_stats (mk, CSTRING, Memc[cmd], SZ_LINE)
	        call printf ("%s = %s %s\n")
		    call pargstr (KY_CIRCLES)
		    if (Memc[cmd] == EOS)
			call pargstr ("0")
		    else
		        call pargstr (Memc[cmd])
		    call pargstr ("pixels")
	    } else
		call mk_sets (mk, CSTRING, Memc[cmd])

	case MKCMD_RECTANGLES:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call gargr (rval)
	    if (Memc[cmd] == EOS) {
	        call mk_stats (mk, RSTRING, Memc[cmd], SZ_LINE)
	        call printf ("%s = %s %g\n")
		    call pargstr (KY_RECTANGLE)
		    if (Memc[cmd] == EOS)
			call pargstr ("0")
		    else
		        call pargstr (Memc[cmd])
		    call pargr (mk_statr (mk, RATIO))
	    } else {
		call mk_sets (mk, RSTRING, Memc[cmd])
		if (nscan () < 3)
		    call mk_setr (mk, RATIO, 1.0)
		else
		    call mk_setr (mk, RATIO, rval)
	    }

	case MKCMD_SHOW:
	    call mk_show (mk)

	case MKCMD_SAVE:
	    iferr {

		# Check that the sizes agree.
		if (sim == NULL) {
		    call mktemp ("scratch", Memc[cmd], SZ_FNAME)
		    sim = immap (Memc[cmd], NEW_COPY, im)
		} else if (IM_LEN(im,1) != IM_LEN(sim,1) || IM_LEN(im,2) !=
		    IM_LEN(sim,2)) {
		    call strcpy (IM_HDRFILE(sim), Memc[cmd], SZ_FNAME)
		    call imunmap (sim)
		    call imdelete (Memc[cmd])
		    call mktemp ("scratch", Memc[cmd], SZ_FNAME)
		    sim = immap (Memc[cmd], NEW_COPY, im)
		}

		# Copy the image.
		call printf ("Saving frame: %d - ")
		    call pargi (mk_stati (mk, FRAME))
		call flush (STDOUT)
		call mk_imcopy (im, sim)

	    } then {
		call erract (EA_WARN)
		call printf ("\n")
	    } else {
		call printf ("done\n")
	    }

	case MKCMD_RESTORE:
	    if (sim == NULL) {
		call printf ("Use :save to define a scratch image.\n")
	    } else if (IM_LEN(sim,1) != IM_LEN(im,1) || IM_LEN(sim,2) !=
	        IM_LEN(im,2)) {
		call printf (
		    "Scatch image and the frame buffer have different sizes.\n")
	    } else {
	        iferr {
		    call printf ("Restoring frame: %d - ")
		        call pargi (mk_stati (mk, FRAME))
		    call flush (STDOUT)
		    call mk_imcopy (sim, im)
	        } then {
		    call erract (EA_WARN)
		    call printf ("\n")
	        } else {
		    call printf ("done\n")
	        }
	    }
	    

	default:
	    call printf ("Unrecognized or ambiguous colon command.\7\n")
	}

	call sfree (sp)
end
