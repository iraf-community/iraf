include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/find.h"

# AP_FDCOLON -- Process colon commands from the daofind task.

procedure ap_fdcolon (ap, im, out, stid, cmdstr, newbuf, newfit)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the iraf image
int	out		# output file descriptor
int	stid		# output file sequence number
char	cmdstr		# command string
int	newbuf		# new center buffer
int	newfit		# new center fit

int	cl, junk
pointer	sp, incmd, outcmd
int	strdic()

begin
	call smark (sp)
	call salloc (incmd, SZ_LINE, TY_CHAR)
	call salloc (outcmd, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[incmd], SZ_LINE)
	if (Memc[incmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command making sure that the pointer to the
	# coords file is always NULL.
	if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, APCMDS) != 0) {
	    cl = NULL
	    call ap_apcolon (ap, im, cl, out, stid, junk, cmdstr, junk, junk,
		junk, junk, newbuf, newfit)
	    if (cl != NULL) {
		call close (cl)
		cl = NULL
		call apsets (cl, CLNAME, "")
	    }
	} else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0) {
	    call ap_nscolon (ap, im, out, stid, cmdstr, junk, junk,
		junk, junk, newbuf, newfit)
	} else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, FCMDS) != 0) {
	    call ap_fcolon (ap, out, stid, cmdstr, newbuf, newfit)
	} else {
	    call ap_fimcolon (ap, out, stid, cmdstr, newbuf, newfit)
	}

	call sfree (sp)
end


# AP_FIMCOLON --  Process colon commands for the daofind task that do
# not affect the data dependent or find parameters.

procedure ap_fimcolon (ap, out, stid, cmdstr, newbuf, newfit)

pointer	ap			# pointer to the apphot structure
int	out			# output file descriptor
int	stid			# output sequence number
char	cmdstr[ARB]		# command string
int	newbuf, newfit		# finding parameters

int	ncmd
pointer	sp, cmd
int	strdic()

begin
	# Get the command.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MISC1)
	switch (ncmd) {
	case ACMD_SHOW:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, FSHOWARGS)
	    switch (ncmd) {
	    case FCMD_DATA:
		call printf ("\n")
		call ap_nshow (ap)
		call printf ("\n")
	    case FCMD_FIND:
		call printf ("\n")
	        call ap_fshow (ap)
		call printf ("\n")
	    default:
		call printf ("\n")
	        call ap_fdshow (ap)
		call printf ("\n")
	    }
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
