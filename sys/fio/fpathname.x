# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<knet.h>
include	<fio.h>

# FPATHNAME -- Return the full OS pathname for a vfn.  If no vfn is given
# (null string), the pathname of the current directory is returned.  Do not
# try to make the pathname into a directory name; if it already is a directory
# name, however, it will remain so.

procedure fpathname (vfn, output_pathname, maxchars)

char	vfn[ARB]			# VFN of file
char	output_pathname[maxchars]	# pathname of file
int	maxchars

int	status
include	<fio.com>
errchk	filerr

begin
	status = OK
	
	if (vfn[1] == EOS)
	    call strpak (vfn, pathname, SZ_PATHNAME)
	else iferr (call fmapfn (vfn, pathname, SZ_PATHNAME))
	    status = ERR

	if (status != ERR) {
	    call strupk (pathname, pathname, SZ_PATHNAME)
	    call zfpath (pathname, output_pathname, maxchars, status)
	}

	if (status == ERR)
	    call filerr (vfn, SYS_FPATHNAME)
end
