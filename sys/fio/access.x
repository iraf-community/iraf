# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<ctype.h>
include	<config.h>
include	<fio.h>
include	<fset.h>

# ACCESS -- Determine the accessiblity of a file.  Use "access(file,0,0)"
# to determine if a file exists.  Specify the mode and/or type to see if
# the file is accessible in a certain mode, and to verify the type of the file.

int procedure access (fname, mode, type)

char	fname[ARB]		# filename
int	mode			# file access mode (0 if dont care)
int	type			# file type (txt|bin) (0 if dont care)

int	zmode, status, fd, ip
int	fstati(), fstdfile()
include	<fio.com>
include	"mmap.inc"
errchk	fmapfn
define	exit_ 91

begin
	status = NO

	# Ignore any whitespace at the beginning of the filename.
	for (ip=1;  IS_WHITE (fname[ip]);  ip=ip+1)
	    ;

	# Special handling is required for the pseudofiles STDIN, STDOUT, etc.
	if (fname[ip] == 'S') {
	    if (fstdfile (fname[ip], fd) == YES) {
		if (mode == 0 || mode == fstati (fd, F_MODE))
		    if (type == 0 || type == fstati (fd, F_TYPE)) {
			status = YES
			goto exit_
		    }
		goto exit_
	    }
	}

	# Regular files.  If the filename cannot be mapped the file does not
	# exist (or the filename mapping file is lost or unreadable).

	iferr (call fmapfn (fname[ip], pathname, SZ_PATHNAME))
	    goto exit_

	zmode = mode
	if (mode >= READ_ONLY && mode <= TEMP_FILE)
	    zmode = mmap[mode]
	call zfacss (pathname, zmode, type, status)

exit_
	return (status)
end
