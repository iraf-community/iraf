# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	"oif.h"

# OIF_OPEN -- Open/create an image.

procedure oif_open (im, o_im,
	root, extn, ksection, cl_index, cl_size, acmode, status)

pointer	im			# image descriptor
pointer	o_im			# old image, if new_copy image
char	root[ARB]		# root image name
char	extn[ARB]		# extension, if any
char	ksection[ARB]		# NOT USED
int	cl_index		# NOT USED
int	cl_size			# NOT USED
int	acmode			# access mode
int	status

pointer	sp, fname
int	hfd, nchars, mode, n
int	open(), read()
bool	strne()
define	err_ 91

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# This image format does not directly support cl_index format.
	if (cl_index > 1)
	    goto err_

	# Determine access mode for header file.
	if (acmode == NEW_COPY || acmode == NEW_IMAGE)
	    mode = NEW_FILE
	else
	    mode = acmode

	# Generate full header file name; the extension may be either ".imh"
	# or nothing, and was set earlier by oif_access().

	if (mode == NEW_FILE && extn[1] == EOS)
	    call iki_mkfname (root, OIF_HDREXTN, Memc[fname], SZ_PATHNAME)
	else
	    call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)

	# Open the image header file.
	iferr (hfd = open (Memc[fname], mode, BINARY_FILE))
	    goto err_

	# If opening an existing image, read the OIF fixed format binary
	# image header into the image descriptor.

	if (mode != NEW_FILE) {
	    iferr {
		call seek (hfd, BOFL)
		nchars = IM_LENHDRMEM(im) * SZ_STRUCT
		n = read (hfd, IM_MAGIC(im), nchars)
	    } then
		n = ERR

	    # Verify that the named file really is an image header.
	    if (n < LEN_IMHDR * SZ_STRUCT || strne (IM_MAGIC(im), "imhdr")) {
		call close (hfd)
		goto err_
	    }
	}

	# If the image was opened read only, close the header file as we will
	# not be accessing it again.

	if (mode == READ_ONLY) {
	    call close (hfd)
	    hfd = NULL
	}

	call strcpy (Memc[fname], IM_HDRFILE(im), SZ_IMHDRFILE)
	IM_HFD(im) = hfd

	status = OK
	call sfree (sp)
	return
err_
	status = ERR
	call sfree (sp)
end
