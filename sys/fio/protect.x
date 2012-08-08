# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<protect.h>
include	<config.h>
include	<syserr.h>
include	<knet.h>
include	<fio.h>

# PROTECT -- Protect a file from deletion.  The recognized action codes are
# defined in <protect.h> and are used to set, remove, or query file
# protection.

int procedure protect (fname, action)

char	fname[ARB]		# file name
int	action			# protect action (prot, unprot, query)

bool	fnullfile()
int	status, access()
errchk	filerr, fmapfn
include	<fio.com>

begin
	# The null file "dev$null" is a special case; ignore attempts to
	# alter the protection of this file.

	if (fnullfile (fname))
	    if (action == QUERY_PROTECTION)
		return (YES)
	    else
		return (OK)

	call fmapfn (fname, pathname, SZ_PATHNAME)
	call zfprot (pathname, action, status)

	if (status == ERR) {
	    if (access (fname,0,0) == YES) {
		switch (action) {
		case SET_PROTECTION:
		    call filerr (fname, SYS_FPROTECT)
		case REMOVE_PROTECTION:
		    call filerr (fname, SYS_FUNPROTECT)
		default:
		    # If the file exists but we cannot query its protection,
		    # better to indicate that it is protected than to abort.
		    return (YES)
		}
	    } else if (access (fname, 0, DIRECTORY_FILE) == YES) {
		switch (action) {
		case SET_PROTECTION:
		    return (OK)			# directory files are protected
		case REMOVE_PROTECTION:
		    call filerr (fname, SYS_FUNPROTECT)
		default:
		    return (YES)
		}
	    } else
		call filerr (fname, SYS_FPROTNEXFIL)
	} else
	    return (status)
end
