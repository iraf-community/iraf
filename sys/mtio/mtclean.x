# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<finfo.h>
include	<xalloc.h>
include	"mtio.h"

# MTCLEAN -- Clean the lock file area.  This routine is called periodically
# by the system (e.g. during CL startup) to scan the lock file area and
# deleted old lock files.  This prevents old lock files which are no longer
# valid from erroneously being used to indicate the current tape position.
#
# What mtclean does is similar to "delete tmp$mt*.lok" except that, since all
# users store lok files in the same area, we don't want to delete all the
# files indiscriminately.  The default action is to delete only files for
# unallocated devices, or for devices allocated to the current user for which
# the lok file is more than STALE seconds old.  It is harmless to delete a
# lok file unnecessarily in that MTIO will recover automatically, but doing
# so will force the tape to be rewound to regain a known position.

procedure mtclean (level, stale, out)

int	level			#I 0 for default; 1 to delete all .lok files
int	stale			#I delete lok file if older than stale seconds
int	out			#I if nonzero, print messages to this file

int	fd, status
pointer	list, ip, cp, device
pointer	sp, fname, owner, template, lbuf
long	fi[LEN_FINFO]

pointer	fntopn()
long	clktime()
int	gstrmatch(), xdevowner()
int	open(), getline(), finfo(), fntgfn()
define	del_ 91

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (owner, SZ_FNAME, TY_CHAR)
	call salloc (template, SZ_FNAME, TY_CHAR)

	# Get file name template matching all lock files.
	call sprintf (Memc[template], SZ_FNAME, "%s%s*%s")
	    call pargstr (LOCKLDIR)
	    call pargstr (LOCKFILE)
	    call pargstr (LOCKEXTN)

	# Open a file list.
	list = fntopn (Memc[template])

	# Examine each file in turn and delete if delete criteria satisfied.
	while (fntgfn (list, Memc[fname], SZ_FNAME) != EOF) {

	    # If level is nonzero (force-delete) delete unconditionally.
	    if (level != 0)
		goto del_

	    # Open lok file and get device name.
	    iferr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE))
		next
	    if (getline (fd, Memc[lbuf]) == EOF) {
		call close (fd)
		goto del_
	    }
	    if (gstrmatch (Memc[lbuf], "unit ", status, ip) <= 0) {
		call close (fd)
		goto del_
	    }
	    device = lbuf + ip
	    for (cp=device;  Memc[cp] != EOS && Memc[cp] != ' ';  cp=cp+1)
		;
	    Memc[cp] = EOS
	    call close (fd)

	    # Determine if the device is currently allocated.  If the device
	    # is not allocated delete the lok file unconditionally.  If the
	    # device is allocated to someone else leave the lok file alone.
	    # If the lok file is allocated to the current user delete it if
	    # the file is older than the stale value.

	    status = xdevowner (Memc[device], Memc[owner], SZ_FNAME)
	    switch (status) {
	    case DV_DEVFREE:
		goto del_

	    case DV_DEVINUSE:
		# Do nothing.

	    case DV_DEVALLOC:
		# Delete the file if older than the stale value.
		if (finfo (Memc[fname], fi) == ERR)
		    goto del_

		if (clktime(FI_MTIME(fi)) > stale) {
		    # Delete the file.
del_		    if (out != NULL) {
			call fprintf (out, "delete lok file %s\n")
			    call pargstr (Memc[fname])
		    }
		    iferr (call delete (Memc[fname]))
			;
		}
	    }
	}

	call fntcls (list)
	call sfree (sp)
end
