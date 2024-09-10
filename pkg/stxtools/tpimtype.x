include <ctype.h>
include "template.h"

define	MAXEXT		25

# TP_IMTYPE -- Determine image type from image extension
#
# B.Simon	02-Oct-98	Original

int procedure tp_imtype (root)

int	root[ARB]	# i: image extension
#--
int	loadext
pointer	extlist[MAXEXT]
pointer	extbuf

data	loadext  / NO /

int	nc, iext, imtype
pointer	sp, ext

int	fnextn(), strdic(), iki_validextn()

begin
	call smark (sp)
	call salloc (ext, SZ_FNAME, TY_CHAR)

	if (loadext == NO) {
	    call tp_loadext (extlist, extbuf)
	    loadext = YES
	}

	nc = fnextn (root, Memc[ext], SZ_FNAME)
	iext = iki_validextn (0, Memc[ext])

	if (iext == 0) {
	    imtype = TP_UNKNOWN

	} else {
	    call strcpy (Memc[extlist[iext]], Memc[ext], SZ_FNAME)
	    imtype = strdic (Memc[ext], Memc[ext], SZ_FNAME, TP_EXT_LIST)
	}

	call sfree (sp)
	return (imtype)
end

# TP_LOADEXT -- Load list of image kernel names indexed by extension

procedure tp_loadext (extlist, extbuf)

pointer	extlist[MAXEXT]		# o: pointers to kernel names
int	extbuf			# o: string buffer containing names
#--
int	fd, flags, taglen, iext, ic, jc, nc
pointer	sp, line, jstr, kstr

string	kernel_tag  "installed kernels "

int	open(), strlen(), getline(), strncmp(), ctoi()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Initialize the image kernel tables

	call iki_init ()

	# Call the kernel debug routine to dump the information
	# about which kernels are associated with which indices

	fd = open ("tp_spool", READ_WRITE, SPOOL_FILE)
	call iki_debug ("Kernel Names", fd, flags)

	# Search the file for the line containing the image kernel info

	call seek (fd, BOF)
	taglen = strlen (kernel_tag)

	while (getline (fd, Memc[line]) != EOF) {
	   if (strncmp (Memc[line], kernel_tag, taglen) != 0)
	       next

	    # Parse the line to extract the info

	    call malloc (extbuf, strlen (Memc[line+taglen]), TY_CHAR)
	    jstr = extbuf
	    kstr = extbuf

	    for (ic = taglen; Memc[line+ic] != EOS; ic = ic + 1) {
		if (Memc[line+ic] == '=') {
		    Memc[jstr] = EOS
		    jstr = jstr + 1

		    jc = 1
		    nc = ctoi (Memc[line+ic+1], jc, iext)
		    ic = ic + 1

		    extlist[iext] = kstr
		    kstr = jstr

		} else if (! IS_WHITE (Memc[line+ic])) {
		    Memc[jstr] = Memc[line+ic]
		    jstr = jstr + 1
		}
	    }

	    break
	}

	call close (fd)
	call sfree (sp)
end
 
