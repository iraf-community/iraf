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

pointer	n_im			#I descriptor of new image (to be filled in)
char	image[ARB]		#I name of image or cl_index to be opened
char	ksection[ARB]		#I information to be passed on to kernel
int	cl_index		#I index of image within cl_index
int	cl_size			#I number of images in cl_index
int	acmode			#I access mode
pointer	o_im			#I existing image descriptor, if new_copy

bool	inherit
pointer	sp, root, extn, textn, fextn
int	status, clmode, i, k
errchk	syserrs, zcalla
include	"iki.com"

begin
	call smark (sp)
	call salloc (root, SZ_PATHNAME, TY_CHAR)
	call salloc (extn, MAX_LENEXTN, TY_CHAR)
	call salloc (textn, MAX_LENEXTN, TY_CHAR)
	call salloc (fextn, MAX_LENEXTN, TY_CHAR)

	# Compute the access mode for the ACCESS test, below.  If opening an
	# existing image, all we want to do here is test for the existence of
	# the image.  If opening a new image, use new image mode.

	if ((acmode == NEW_IMAGE || acmode == NEW_COPY))
	    clmode = NEW_IMAGE
	else
	    clmode = 0

	# Parse the image name into the root and extn fields.
	call iki_parse (image, Memc[root], Memc[extn])

	# If we are opening a new image and an explicit extension is given
	# this determines the type of image to be created.  Otherwise if we
	# are opening a new copy image and type inheritance is enabled, the
	# new image will be the same type as the old one.  Otherwise (new
	# image, type not specified or inherited) the default image type 
	# specified by the IMTYPE mechanism is used.  If opening an existing
	# image the access method of each image kernel is called until a
	# kernel recognizes the image.

	repeat {
	    # Is type inheritance permitted?
	    inherit = (k_inherit == YES)
	    if (inherit && acmode == NEW_COPY)
		inherit = (and (IKI_FLAGS(IM_KERNEL(o_im)), IKF_NOCREATE) == 0)

	    # Select the kernel to be used.
	    if (acmode == NEW_COPY && Memc[extn] == EOS && inherit) {
		# Inherit the same type as an existing image.
		k = IM_KERNEL(o_im)
		break

	    } else if (clmode == NEW_IMAGE && Memc[extn] == EOS) {
		# Use the default type for new images.
		k = k_defimtype
		break

	    } else {
		# Select an image kernel by calling the access function in each
		# loaded kernel until somebody claims the image.  In the case
		# of a new image, the access function tests only the legality
		# of the extn.  If no extn is given but the imagefile has an
		# extension, the access procedure will fill in the extn field.

		k = 0
		for (i=1;  i <= k_nkernels;  i=i+1) {
		    call strcpy (Memc[extn], Memc[textn], MAX_LENEXTN)
		    call zcall5 (IKI_ACCESS(i), i, Memc[root], Memc[textn],
			clmode, status)

		    if (status == YES) {
			if (k == 0) {
			    # Stop on the first match if an explicit extension
			    # was given.

			    k = i
			    call strcpy (Memc[textn], Memc[fextn], MAX_LENEXTN)
			    if (Memc[extn] != EOS)
				break

			} else if (Memc[extn] == EOS) {
			    # If no extension was given and we match multiple
			    # files then we have an ambiguous name and k=ERR.

			    k = ERR
			    break
			}
		    }
		}

		# Update the selected extn field.
		if (k > 0)
		    call strcpy (Memc[fextn], Memc[extn], MAX_LENEXTN)

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

	# The image name is ambiguous; we don't know which image to open.
	# This can only happen when opening an existing image and multiple
	# images exist matching the name given.  It is permissible to create
	# multiple images with the same name but different types.

	if (k == ERR)
	    call syserrs (SYS_IKIAMBIG, IM_NAME(n_im))

	# Illegal image type or image does not exist.
	if (k == 0) {
	    if (acmode == NEW_IMAGE || acmode == NEW_COPY)
		call syserrs (SYS_IKIEXTN, IM_NAME(n_im))
	    else
		call syserrs (SYS_IKIOPEN, IM_NAME(n_im))
	}

	# Set the image kernel (format) to be used.
	IM_KERNEL(n_im) = k

	# Open/create the image.  Save the kernel index in the image header
	# so that subsequent IKI routines know which kernel to use.

	call zcalla (IKI_OPEN(k), k, n_im, o_im, Memc[root], Memc[extn],
	    ksection, cl_index, cl_size, acmode, status)
	if (status == ERR)
	    call syserrs (SYS_IKIOPEN, IM_NAME(n_im))

	call sfree (sp)
end
