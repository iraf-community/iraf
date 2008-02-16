# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include "iki.h"

# IKI_ACCESS -- Determine if the named image exists, and if so, return the
# the index of the IKI kernel to be used to access the image, else return 0 if
# the named image is not found.  If multiple images exist with the same name
# (e.g. but with different image types) then ERR is returned.  An NEW_IMAGE
# access mode may be specified to associate an extension with an image kernel
# without testing for the existence of an image.  If the input image name did
# not specify an extension or what appeared to be an extension was just a .
# delimited field, we will patch up the ROOT and EXTN strings to the real
# values.

int procedure iki_access (image, root, extn, acmode)

char	image[ARB]		#I image/group name
char	root[ARB]		#O image/group file name
char	extn[ARB]		#O image/group file extension
int	acmode

bool	first_time
int	i, k, status, op
pointer	sp, osroot, fname, textn, fextn, ip
data	first_time /true/

bool	fnullfile()
int	gstrcpy(), strlen()
errchk	fpathname, syserrs
include	"iki.com"

begin
	call smark (sp)
	call salloc (osroot, SZ_PATHNAME, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (textn, MAX_LENEXTN, TY_CHAR)
	call salloc (fextn, MAX_LENEXTN, TY_CHAR)

	# The first call makes sure the IKI kernels are loaded into the kernel
	# table.

	if (first_time) {
	    call iki_init()
	    first_time = false
	}

	call iki_parse (image, root, extn)
	if (fnullfile (root)) {
	    call sfree (sp)
	    return (1)
	}

	repeat {
	    # Convert to absolute pathname to render names like file
	    # and ./file equivalent.  Add a dummy file extension first
	    # to cause escape sequence encoding of any .DDDD etc. files
	    # which may be part of the root image name.

	    op = gstrcpy (root, Memc[fname], SZ_PATHNAME)
	    call strcpy (".x", Memc[fname+op], SZ_PATHNAME-op+1)
	    call fpathname (Memc[fname], Memc[osroot], SZ_PATHNAME)
	    Memc[osroot+strlen(Memc[osroot])-2] = EOS

	    # Escape any $ in the pathname since we are still in VOS land.
	    op = 1
	    for (ip=osroot;  Memc[ip] != EOS;  ip=ip+1) {
		if (Memc[ip] == '$' || Memc[ip] == '[') {
		    root[op] = '\\'
		    op = op + 1
		}
		root[op] = Memc[ip]
		op = op + 1
	    }

	    root[op] = EOS

	    # Select an image kernel by calling the access function in each
	    # loaded kernel until somebody claims the image.  If multiple
	    # kernels claim the image the image specification (name) is
	    # ambiguous and we don't know which image was intended, an error.
	    # Note that in the case of a new image, the access function
	    # tests only the legality of the extn.

	    k = 0
	    for (i=1;  i <= k_nkernels;  i=i+1) {
		call strcpy (extn, Memc[textn], MAX_LENEXTN)
		call zcall5 (IKI_ACCESS(i), i,root,Memc[textn],acmode,status)

		if (status == YES) {
		    if (k == 0) {
			# Stop on the first access if an explicit extension
			# was given.

			k = i
			call strcpy (Memc[textn], Memc[fextn], MAX_LENEXTN)
			if (extn[1] != EOS)
			    break

		    } else {
			# The image name is ambiguous.
			call sfree (sp)
			return (ERR)
		    }
		}
	    }

	    # Valid image using kernel K.
	    if (k != 0) {
		call strcpy (Memc[fextn], extn, MAX_LENEXTN)
		call sfree (sp)
		return (k)
	    }

	    # If the search failed and an extension was given, maybe what
	    # we thought was an extension was really just part of the root
	    # filename.  Try again with the extn folded into the root.

	    if (status == NO && extn[1] != EOS) {
		call strcpy (image, root, SZ_PATHNAME)
		extn[1] = EOS
	    } else
		break
	}

	call sfree (sp)
	return (0)
end
