# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <imhdr.h>
include <imio.h>
include "iki.h"

# IKI_OPEN -- Open or create an image.  If opening an existing image, determine
# the type of image and open it with the corresponding kernel.  If creating a
# new image, the value of the environment variable IMTYPE determines the type
# of image to be created, i.e., the kernel to be used to open the image.  If
# opening a new copy image, create an image of the same type as the existing
# image.

procedure iki_open (n_im, image, ksection, cl_index, cl_size, acmode, o_im)

pointer	n_im			# descriptor of new image (to be filled in)
char	image[ARB]		# name of image or cl_index to be opened
char	ksection[ARB]		# information to be passed on to kernel
int	cl_index		# index of image within cl_index
int	cl_size			# number of images in cl_index
int	acmode			# access mode
pointer	o_im			# existing image descriptor, if new_copy

bool	can_inherit_type
pointer	sp, root, extn
int	status, cluster_mode, k
int	envfind()
include	"iki.com"
errchk	syserrs

begin
	call smark (sp)
	call salloc (root, SZ_PATHNAME, TY_CHAR)
	call salloc (extn, MAX_LENEXTN, TY_CHAR)

	# Compute the access mode for the ACCESS test, below.  If opening an
	# existing image, all we want to do here is test for the existence of
	# the image.  If opening a new image, use new image mode.

	if ((acmode == NEW_IMAGE || acmode == NEW_COPY))
	    cluster_mode = NEW_IMAGE
	else
	    cluster_mode = 0		# test for existence of cluster

	# Parse the image name into the root and extn fields.
	call iki_parse (image, Memc[root], Memc[extn])

	# If opening a new copy image, make it the same type as the old image
	# unless an explicit extension was given.  If we are opening a new
	# image and no extension was given, check the environment variable
	# IMTYPE to see what type of image to create.  If opening an existing
	# image, call the access routines to determine the image type.  The
	# image type is defined by the filename extension if given.

	repeat {
	    if (acmode == NEW_COPY) {
		k = IM_KERNEL(o_im)
		can_inherit_type = (and (IKI_FLAGS(k), IKF_NOCREATE) == 0)
	    } else
		can_inherit_type = true

	    if (acmode == NEW_COPY && Memc[extn] == EOS && can_inherit_type) {
		k = IM_KERNEL(o_im)
		break
	    } else {
		if (cluster_mode == NEW_IMAGE)
		    if (Memc[extn] == EOS)
			if (envfind (ENV_DEFIMTYPE,Memc[extn],MAX_LENEXTN) <= 0)
			    call strcpy (DEF_IMTYPE, Memc[extn], MAX_LENEXTN)

		# Select an image kernel by calling the access function in each
		# loaded kernel until somebody claims the image.  Note that in
		# the case of a new image, the access function tests only the
		# legality of the extn.  If no extn is given but the imagefile
		# has an extension, the access procedure will fill in the extn
		# field.

		for (k=1;  k <= k_nkernels;  k=k+1) {
		    call zcall4 (IKI_ACCESS(k), Memc[root], Memc[extn],
			cluster_mode, status)
		    if (status == YES)
			break
		}

		if (k > k_nkernels)
		    k = 0

		# If the search failed and an extension was given, maybe what
		# we thought was an extension was really just part of the root
		# filename.  Try again with the extn folded into the root.

		if (k == 0 && Memc[extn] != EOS) {
		    call strcpy (image, Memc[root], SZ_PATHNAME)
		    Memc[extn] = EOS
		} else
		    break
	    }
	}

	# Illegal image type or image does not exist.
	if (k == 0)
	    if (acmode == NEW_IMAGE || acmode == NEW_COPY)
		call syserrs (SYS_IKIEXTN, IM_NAME(n_im))
	    else
		call syserrs (SYS_IKIOPEN, IM_NAME(n_im))

	IM_KERNEL(n_im) = k

	# Open/create the image.  Save the kernel index in the image header
	# so that subsequent IKI routines know which kernel to use.

	call zcall9 (IKI_OPEN(k), n_im, o_im, Memc[root], Memc[extn],
	    ksection, cl_index, cl_size, acmode, status)
	if (status == ERR)
	    call syserrs (SYS_IKIOPEN, IM_NAME(n_im))

	call sfree (sp)
end
