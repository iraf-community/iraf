include	<ctype.h>
include <imio.h>
include <imhdr.h>
include "gf.h"

define	MAX_KERNEL	25
define	LEN_TAG		18

#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types
#* B.Simon	29-Nov-99	Modified calling sequence for gf_filetype
#* B.Simon	20-Nov-00	get default extension with gf_getext

# GF_IMTYPE -- Determine image type from name stored in image descriptor

int procedure gf_imtype (im)

pointer	im		# i: image descriptor
#--
int	gf_filetype()

begin
	return (gf_filetype (IM_HDRFILE(im), IM_ACMODE(im)))
end

# GF_FILETYPE -- Determine image type from image extension

int procedure gf_filetype (image, acmode)

char	image[ARB]	# i: image name
int	acmode		# i: image access mode
#--
pointer	kname[MAX_KERNEL]
pointer	kbuf
int	nkernel

int	ikern, code
pointer	sp, type, ext

int	strdic(), iki_validextn()

begin
	call smark (sp)
	call salloc (type, SZ_SHORTSTR, TY_CHAR)
	call salloc (ext, SZ_SHORTSTR, TY_CHAR)

	# Load the table of image types

	if (nkernel == 0)
	    call gf_initype (kname, kbuf, nkernel)

	# Get the image extension from the image name

	call gf_getext (image, acmode, Memc[ext], SZ_SHORTSTR)

	# Get the image type from the extension

	ikern = iki_validextn (0, Memc[ext])

	if (ikern == 0) {
	    code = 0
	} else {
	    call strcpy (Memc[kname[ikern]], Memc[type], SZ_SHORTSTR)
	    code = strdic (Memc[type], Memc[type], SZ_SHORTSTR, TYPE_LIST)
	}

	call sfree (sp)
	return (code)
end

# GF_INITYPE -- Initialize array of kernel types
#
# This function calls iki_debug and must be modified if this function is
# modified.

procedure gf_initype (kname, kbuf, nkernel)

pointer	kname[MAX_KERNEL]	# o: pointers to type strings
pointer	kbuf			# o: buffer containing type strings
int	nkernel			# o: number of kernel types
#--
int	fd, nc, ic, jc , flags
pointer	sp, line, jstr, kstr

int	open(), ctoi(), getline(), strncmp(), strlen()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Load the image kernel table if it has not previously been loaded

	call iki_init ()

	# The iki procedure that identify an image type return an integer
	# This is useless for our purposes, so we call the debug procedure
	# that dumps the name of the image kernel togther with its number
	# This is a real kludge, but the best we can do with the current
	# interface

	fd = open ("iki_spool", READ_WRITE, SPOOL_FILE)
	call iki_debug ("Useless Title", fd, flags)

	# Parse the information in the debug file to get the image 
	# kernel names. This is obviously dependent on the format
	# of the debug file and will have to track any changes to it

	call seek (fd, BOF)
	while (getline (fd, Memc[line]) != EOF) {
	    if (strncmp (Memc[line], "installed kernels ", LEN_TAG) == 0) {

		nc = strlen (Memc[line])
		call malloc (kbuf, nc, TY_CHAR)

		jstr = kbuf
		kstr = kbuf

		# The line containing the kernel names has the format
		# "installed kernels name=index ..."

		for (ic = LEN_TAG; Memc[line+ic] != EOS; ic = ic + 1) {
		    if (Memc[line+ic] == '=') {
			Memc[jstr] = EOS
			jstr = jstr + 1

			jc = 1
			nc = ctoi (Memc[line+ic+1], jc, nkernel)
			ic = ic + nc

			kname[nkernel] = kstr
			kstr = jstr

		    } else if (! IS_WHITE (Memc[line+ic])) {
			Memc[jstr] = Memc[line+ic]
			jstr = jstr + 1
		    }
		}

		break
	    }
	}

	call close (fd)
	call sfree (sp)
end

