# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	"imfort.h"
include	"oif.h"

# IMF_MKPIXFNAME -- Generate the pixel file name.  Leave the logical pixfile
# name in the image header, and return the pathname to the pixel file in the
# output argument.

procedure imf_mkpixfname (im, pixfile, maxch, ier)

pointer	im			#I image descriptor	
char	pixfile[maxch]		#O receives pathname to pixfile
int	maxch			#I max chars out
int	ier			#O exit status code

int	status, n
char	suffix[2], hdr[STRLEN_HDR]
pointer	sp, imdir, osdir, root, extn, subdir, fname, ip, op
int	fnroot(), fnldir(), strncmp(), imgdirx()
string	pixextn OIF_PIXEXTN
define	done_ 91

begin
	call smark (sp)
	call salloc (imdir,  SZ_PATHNAME, TY_CHAR)
	call salloc (osdir,  SZ_PATHNAME, TY_CHAR)
	call salloc (root,   SZ_PATHNAME, TY_CHAR)
	call salloc (subdir, SZ_PATHNAME, TY_CHAR)
	call salloc (fname,  SZ_PATHNAME, TY_CHAR)
	call salloc (extn,   SZ_FNAME, TY_CHAR)

	ier = OK

	# Get the logical directory where the pixel file goes.
	n = imgdirx (Memc[imdir], SZ_PATHNAME)

	# If the imdir name begins with "HDR$", put the pixfile in same
	# directory as the header or in a subdirectory, else put the pixel
	# file in the named directory.  If the pixel file goes in a HDR
	# subdirectory, create the directory if it does not already exist.
	# For IMFORT programs which are subject to the whims of the host
	# system, be a little forgiving about the case of the HDR$.

	call strcpy (Memc[imdir], hdr, STRLEN_HDR)
	call strupr (hdr)

	if (strncmp (hdr, HDR, STRLEN_HDR) == 0) {
	    call amovc (HDR, Memc[imdir], STRLEN_HDR)

	    ip = imdir + STRLEN_HDR
	    for (op=subdir;  Memc[ip] != EOS && Memc[ip] != '/';  ip=ip+1) {
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	    Memc[op] = EOS
	    
	    # Make the subdirectory if it does not already exist.
	    if (Memc[subdir] != EOS) {
		n = fnldir (IM_HDRFILE(im), Memc[osdir], SZ_PATHNAME)
		call zfpath (Memc[osdir], Memc[fname], SZ_PATHNAME, n)
		call zfsubd (Memc[fname], SZ_PATHNAME, Memc[subdir], n)

		call strpak (Memc[fname], Memc[fname], SZ_PATHNAME)
		call zfacss (Memc[fname], 0, DIRECTORY_FILE, status)

		if (status == NO) {
		    call zfmkdr (Memc[fname], status)
		    if (status == ERR) {
			ier = IE_MKDIR
			goto done_
		    }
		}
	    }
	} else
	    call zfpath (Memc[imdir], Memc[imdir], SZ_PATHNAME, n)

	# Make up the root name of the new pixel file.
	if (fnroot (IM_HDRFILE(im), Memc[fname], SZ_PATHNAME) <= 0)
	    call strcpy (pixextn, Memc[fname], SZ_PATHNAME)
	call strcat (".", Memc[fname], SZ_PATHNAME)
	call strcat (pixextn, Memc[fname], SZ_PATHNAME)
	call imf_trans (Memc[fname], Memc[root], Memc[extn])

	# Get a unique pixel file name.  If a file with the default pixel
	# file name already exists in the current IMDIR, a suffix is found
	# for the file which results in a unique file name (there is a
	# concurrency loophole in this which can cause the uniqueness
	# constraint to fail, but this is unlikely).

	suffix[1] = 'a'
	suffix[2] = 'a'
	suffix[3] = EOS

	for (n=0;  ;  n=n+1) {
	    # Construct filename "imdir$root.pix".
	    call strcpy (Memc[imdir], IM_PIXFILE(im), SZ_PATHNAME)
	    call strcat (Memc[root], IM_PIXFILE(im), SZ_PATHNAME)
	    call strcat (".", IM_PIXFILE(im), SZ_PATHNAME)
	    call strcat (pixextn, IM_PIXFILE(im), SZ_PATHNAME)

	    call imf_gpixfname (IM_PIXFILE(im), IM_HDRFILE(im), pixfile, maxch)

	    # Ensure that the filename is unique.
	    call strpak (pixfile, Memc[fname], SZ_PATHNAME)
	    call zfacss (Memc[fname], 0, 0, status)

	    if (status == YES) {
		if (n == 0) {
		    for (op=root;  Memc[op] != EOS;  op=op+1)
			;
		} else {
		    if (suffix[2] == 'z') {
			suffix[2] = 'a'
			if (suffix[1] == 'z') {
			    ier = IE_PFNNUNIQ
			    goto done_
			} else
			    suffix[1] = suffix[1] + 1
		    } else
			suffix[2] = suffix[2] + 1
		}

		call strcpy (suffix, Memc[op], 2)
	    } else
		break
	}

done_
	# Set the error message operand name if an error occurred.
	if (ier != OK)
	    call im_seterrop (ier, IM_HDRFILE(im))

	call sfree (sp)
end
