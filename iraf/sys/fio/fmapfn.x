# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<fio.h>

# FMAPFN -- Map the VFN of an existing file to a packed OSFN.  ERR is returned
# if there is insufficient information in the VFN database to map the filename.
# OK is returned if the mapping can be performed, but a status of OK does not
# imply that the named file exists.

procedure fmapfn (vfn, osfn, maxch)

char	vfn[ARB]		# virtual filename of file to be mapped.
char	osfn[maxch]		# packed OS filename (output)
int	maxch

int	status, ip, delim
pointer	vfd, sp, nodename
pointer	vfnopen()
int	vfnmapu(), ki_gnode()
errchk	vfnopen, vfnmapu, syserrs

begin
	call smark (sp)
	call salloc (nodename, SZ_FNAME, TY_CHAR)

	# Map VFN to OSFN.

	vfd = vfnopen (vfn, READ_ONLY)
	status = vfnmapu (vfd, osfn, maxch)
	call vfnclose (vfd, VFN_NOUPDATE)

	if (status == ERR)
	    call syserrs (SYS_FNOSUCHFILE, vfn)

	# If the file resides on the local node strip the node name, returning
	# a legal host system filename as the result.

	if (ki_gnode (osfn, Memc[nodename], delim) == 0)
	    ip = delim + 1
	else
	    ip = 1

	call osfn_pkfname (osfn[ip], osfn, maxch)
	call sfree (sp)
end
