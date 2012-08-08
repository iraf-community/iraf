include "../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_PCOLON -- Process the PSF fitting colon commands.

procedure dp_pcolon (dao, psfim, opst, psfgr, cmdstr, psf_new, psf_computed,
	psf_written)

pointer	dao			# pointer to the daophot structure
pointer	psfim			# pointer to the output psf image
int	opst			# the output psf star list file descriptor
int	psfgr			# the output psf group file descriptor
char	cmdstr[ARB]		# the input command string
bool	psf_new			# is the psf star list defined ?
bool	psf_computed		# has the psf been fit ?
bool	psf_written		# has the psf been updated ?

bool	bval
int	ncmd, ival
pointer	sp, cmd, str1, str2, str3
real	rval

bool	itob()
int	strdic(), nscan(), btoi(), dp_stati(), open(), dp_fctdecode()
int	dp_pstati(), strlen()
pointer	immap(), tbtopn()
real	dp_statr()
errchk	immap(), open(), tbtopn()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	call salloc (str3, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PSF_CMDS)
	switch (ncmd) {

	case PSFCMD_PSFIMAGE:
	    call gargwrd (Memc[str1], SZ_LINE)
	    call gargwrd (Memc[str2], SZ_LINE)
	    call gargwrd (Memc[str3], SZ_LINE)
	    if (nscan() == 1) {
		call dp_stats (dao, PSFIMAGE, Memc[str1], SZ_LINE) 
		call dp_stats (dao, OUTREJFILE, Memc[str2], SZ_LINE) 
		call dp_stats (dao, OUTPHOTFILE, Memc[str3], SZ_LINE) 
		call printf ("psfim = %s  opstfile = %s  grpfile = %s\n")
		    call pargstr (Memc[str1])
		    call pargstr (Memc[str2])
		    call pargstr (Memc[str3])
	    } else if (nscan() == 4) {
		if (psfim != NULL)
		    call imunmap (psfim)
		psfim = NULL
		if (opst != NULL) {
		    if (dp_stati (dao, TEXT) == YES)
			call close (opst)
		    else
			call tbtclo (opst)
		}
		opst = NULL
		if (psfgr != NULL) {
		    if (dp_stati (dao, TEXT) == YES)
			call close (psfgr)
		    else
			call tbtclo (psfgr)
		}
		psfgr = NULL
		iferr {
		    psfim = immap (Memc[str1], NEW_IMAGE, dp_pstati (dao,
		        LENUSERAREA))
		    if (dp_stati (dao, TEXT) == YES)
			opst = open (Memc[str2], NEW_FILE, TEXT_FILE)
		    else
			opst = tbtopn (Memc[str2], NEW_FILE, 0)
		    if (dp_stati (dao, TEXT) == YES)
			psfgr = open (Memc[str3], NEW_FILE, TEXT_FILE)
		    else
			psfgr = tbtopn (Memc[str3], NEW_FILE, 0)
		} then {
		    if (psfim != NULL)
		        call imunmap (psfim)
		    psfim = NULL
		    if (opst != NULL) {
		        if (dp_stati (dao, TEXT) == YES)
			    call close (opst)
		        else
			    call tbtclo (opst)
		    }
		    opst = NULL
		    if (psfgr != NULL) {
		        if (dp_stati (dao, TEXT) == YES)
			    call close (psfgr)
		        else
			    call tbtclo (psfgr)
		    }
		    psfgr = NULL
		    call dp_sets (dao, PSFIMAGE, "")
		    call dp_sets (dao, OUTREJFILE, "")
		    call dp_sets (dao, OUTPHOTFILE, "")
		} else {
		    call dp_sets (dao, PSFIMAGE, Memc[str1])
		    call dp_sets (dao, OUTREJFILE, Memc[str2])
		    call dp_sets (dao, OUTPHOTFILE, Memc[str2])
		}
		psf_written = false
	    }

	case PSFCMD_FUNCTION:
	    call gargwrd (Memc[str1], SZ_LINE)
	    if (nscan() == 1) {
		call dp_stats (dao, FUNCLIST, Memc[str1], SZ_LINE) 
		call strcpy (Memc[str1+1], Memc[str2],
		    strlen (Memc[str1]) - 2)
		call printf ("function = %s\n")
		    call pargstr (Memc[str2])
	    } else if (dp_fctdecode (Memc[str1], Memc[str2], SZ_FNAME) > 0) {
		call dp_sets (dao, FUNCLIST, Memc[str2])
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_VARORDER:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("varorder = %d\n")
		    call pargi (dp_stati (dao, VARORDER))
	    } else if (ival >= -1 && ival <= 2) {
		call dp_seti (dao, VARORDER, ival)
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_FEXPAND:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("fexpand = %b\n")
		    call pargb (itob (dp_stati (dao, FEXPAND)))
	    } else {
		call dp_seti (dao, FEXPAND, btoi (bval))
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_PSFRAD:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("psfrad = %g scale units\n")
		    call pargr (dp_statr (dao, SPSFRAD))
	    } else {
		call dp_setr (dao, SPSFRAD, rval)
		call dp_setr (dao, RPSFRAD, rval)
		call dp_setr (dao, PSFRAD, rval / dp_statr (dao, SCALE))
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_FITRAD:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("fitrad = %g scale units\n")
		    call pargr (dp_statr (dao, SFITRAD))
	    } else {
		call dp_setr (dao, SFITRAD, rval)
		call dp_setr (dao, FITRAD, rval / dp_statr (dao, SCALE))
		psf_new = true
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_MATCHRAD:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("matchrad = %g scale units\n")
		    call pargr (dp_statr (dao, SMATCHRAD))
	    } else {
		call dp_setr (dao, SMATCHRAD, rval)
		call dp_setr (dao, MATCHRAD, rval / dp_statr (dao, SCALE))
	    }

	case PSFCMD_NCLEAN:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nclean = %d\n")
		    call pargi (dp_stati (dao, NCLEAN))
	    } else if (ival >= 0) {
		call dp_seti (dao, NCLEAN, ival)
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_SATURATED:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("saturated = %b\n")
		    call pargb (itob (dp_stati (dao, SATURATED)))
	    } else {
		call dp_seti (dao, SATURATED, btoi (bval))
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_SCALE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("scale = %g units per pixel\n")
		    call pargr (dp_statr (dao, SCALE))
	    } else {
		call dp_setr (dao, FWHMPSF, dp_statr (dao, SFWHMPSF) / rval)
		call dp_setr (dao, PSFRAD,  dp_statr (dao, SPSFRAD) / rval)
		call dp_setr (dao, FITRAD, dp_statr (dao, SFITRAD) / rval)
		call dp_setr (dao, MATCHRAD, dp_statr (dao, SMATCHRAD) / rval)
		call dp_setr (dao, SCALE, rval)
		psf_new = true
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_FWHMPSF:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("fwhmpsf = %g scale units\n")
		    call pargr (dp_statr (dao, SFWHMPSF))
	    } else {
		call dp_setr (dao, SFWHMPSF, rval)
		call dp_setr (dao, FWHMPSF, rval / dp_statr (dao, SCALE))
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_DATAMIN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("datamin = %g counts\n")
		    call pargr (dp_statr (dao, MINGDATA))
	    } else {
		call dp_setr (dao, MINGDATA, rval)
		psf_new = true
		psf_computed = false
		psf_written = false
	    }

	case PSFCMD_DATAMAX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("datamax = %g counts\n")
		    call pargr (dp_statr (dao, MAXGDATA))
	    } else {
		call dp_setr (dao, MAXGDATA, rval)
		psf_new = true
		psf_computed = false
		psf_written = false
	    }

	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
