# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"oif.h"

# IMF_GPIXFNAME -- Convert a logical pixfile name into a physical pathname.

procedure imf_gpixfname (pixfile, hdrfile, path, maxch)

char	pixfile[ARB]		# pixfile name
char	hdrfile[ARB]		# header file name (gives hdr directory)
char	path[maxch]		# receives pathname
int	maxch

int	ip, nchars
pointer	sp, fname, op
int	strncmp(), fnldir()

begin
	# Merely return pathname if not case "HDR$".
	if (strncmp (pixfile, HDR, STRLEN_HDR) != 0) {
	    call zfpath (pixfile, path, maxch, nchars)
	    return
	}

	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Get host pathname of pixel file directory.
	nchars = fnldir (hdrfile, Memc[fname], SZ_PATHNAME)
	call zfpath (Memc[fname], path, maxch, nchars)

	# Fold in any subdirectories from the pixfile name.
	# (as in HDR$pixels/).

	op = fname
	nchars = 0

	for (ip=STRLEN_HDR+1;  pixfile[ip] != EOS;  ip=ip+1) {
	    if (pixfile[ip] == '/') {
		Memc[op] = EOS
		call zfsubd (path, maxch, Memc[fname], nchars)
		op = fname
	    } else {
		Memc[op] = pixfile[ip]
		op = op + 1
	    }
	}

	# Tack on the pixel file name, which was left in the fname buf.
	if (op > fname) {
	    Memc[op] = EOS
	    if (nchars > 0)
		call strcpy (Memc[fname], path[nchars+1], maxch-nchars)
	    else
		call strcat (Memc[fname], path, maxch)
	}

	call sfree (sp)
end
