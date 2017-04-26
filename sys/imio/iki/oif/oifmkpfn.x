# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>
include	<knet.h>
include	"oif.h"

# OIF_MKPIXFNAME -- Generate the pixel file name.  Leave the logical pixfile
# name in the image header, and return the pathname to the pixel file in the
# output argument.

procedure oif_mkpixfname (im, pixfile, maxch)

pointer	im			# image descriptor
char	pixfile[maxch]		# receives pathname to pixfile
int	maxch

char	suffix[2]
int	len_osdir, len_root, len_extn, n
pointer	sp, imdir, osdir, root, extn, subdir, fname, ip, op

bool	fnullfile()
int	fnroot(), fnldir(), access(), envgets(), strncmp()
string	pixextn OIF_PIXEXTN
errchk	fmkdir, imerr

begin
	# Clear junk text at the end of the filename.
	call aclrc (IM_PIXFILE(im), SZ_IMPIXFILE)

	# Check for the null image.
	if (fnullfile (IM_HDRFILE(im))) {
	    call strcpy ("dev$null", IM_PIXFILE(im), SZ_IMPIXFILE)
	    call strcpy (IM_PIXFILE(im), pixfile, maxch)
	    return
	}

	call smark (sp)
	call salloc (imdir,  SZ_PATHNAME, TY_CHAR)
	call salloc (osdir,  SZ_PATHNAME, TY_CHAR)
	call salloc (root,   SZ_PATHNAME, TY_CHAR)
	call salloc (subdir, SZ_PATHNAME, TY_CHAR)
	call salloc (fname,  SZ_PATHNAME, TY_CHAR)
	call salloc (extn,   SZ_FNAME, TY_CHAR)

	if (envgets ("imdir", Memc[imdir], SZ_PATHNAME) <= 0)
	    call strcpy (HDR, Memc[imdir], SZ_PATHNAME)

	if (strncmp (Memc[imdir], HDR, STRLEN_HDR) == 0) {
	    # Put pixfile in same directory as the header or in a subdirectory.
	    # In the latter case, create the directory if it does not already
	    # exist.

	    ip = imdir + STRLEN_HDR
	    for (op=subdir;  Memc[ip] != EOS && Memc[ip] != '/';  ip=ip+1) {
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	    Memc[op] = EOS
	    
	    if (Memc[subdir] != EOS) {
		n = fnldir (IM_HDRFILE(im), Memc[fname], SZ_PATHNAME)
		call fpathname (Memc[fname], Memc[fname], SZ_PATHNAME)
		call zfsubd (Memc[fname], SZ_PATHNAME, Memc[subdir], n)
		if (access (Memc[fname], 0, DIRECTORY_FILE) == NO)
		    call fmkdir (Memc[fname])
	    }
	} else
	    call fpathname (Memc[imdir], Memc[imdir], SZ_PATHNAME)

	# Make up the root name of the new pixel file.  Take the root part of
	# the header file and escape sequence encode it.  We have to do this
	# because it is to be concatenated to an OS directory name, which will
	# prevent translation of the root file name during normal filename
	# mapping.

	if (fnroot (IM_HDRFILE(im), Memc[fname], SZ_PATHNAME) <= 0)
	    call strcpy (pixextn, Memc[fname], SZ_PATHNAME)
	call iki_mkfname (Memc[fname], pixextn, Memc[fname], SZ_PATHNAME)
	call vfn_translate (Memc[fname], Memc[osdir], len_osdir,
	    Memc[root], len_root, Memc[extn], len_extn)

	suffix[1] = 'a'
	suffix[2] = 'a'
	suffix[3] = EOS

	for (n=0;  ;  n=n+1) {
	    call sprintf (IM_PIXFILE(im), SZ_PATHNAME, "%s%s.%s")
		call pargstr (Memc[imdir])
		call pargstr (Memc[root])
		call pargstr (pixextn)

	    call oif_gpixfname (IM_PIXFILE(im), IM_HDRFILE(im), pixfile, maxch)

	    # Ensure that the filename is unique.
	    if (access (pixfile, 0,0) == YES) {
		if (n == 0) {
		    for (op=root;  Memc[op] != EOS;  op=op+1)
			;
		} else {
		    if (suffix[2] == 'z') {
			suffix[2] = 'a'
			if (suffix[1] == 'z')
			    call imerr (IM_NAME(im), SYS_FMKTEMP)
			else
			    suffix[1] = suffix[1] + 1
		    } else
			suffix[2] = suffix[2] + 1
		}

		call strcpy (suffix, Memc[op], 2)
	    } else
		break
	}

	call sfree (sp)
end
