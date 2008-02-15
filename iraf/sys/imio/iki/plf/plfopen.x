# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>
include	<pmset.h>
include	<plio.h>


# PLF_OPEN -- Open a PMIO mask on an image descriptor.

procedure plf_open (kernel, im, o_im,
	root, extn, ksection, cl_index, cl_size, acmode, status)

int	kernel			#I IKI kernel
pointer	im			#I image descriptor
pointer	o_im			#I [not used]
char	root[ARB]		#I root image name
char	extn[ARB]		#I filename extension
char	ksection[ARB]		#I QPIO filter expression
int	cl_index		#I [not used]
int	cl_size			#I [not used]
int	acmode			#I [not used]
int	status			#O ok|err

pointer	sp, fname, hp, pl
int	naxes, axlen[IM_MAXDIM], depth
bool	envgetb(), fnullfile()
pointer	pl_open()
int	access()
errchk	imerr

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (hp, IM_LENHDRMEM(im), TY_CHAR)

	# The only valid cl_index for a PL image is -1 (none specified) or 1.
	if (!(cl_index < 0 || cl_index == 1)) {
	    call sfree (sp)
	    status = ERR
	    return
	}

	# Get mask file name.
	call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)
	call aclrc (IM_HDRFILE(im), SZ_IMHDRFILE)
	call strcpy (Memc[fname], IM_HDRFILE(im), SZ_IMHDRFILE)

	# Open an empty mask.
	pl = pl_open (NULL)

	if (acmode == NEW_IMAGE || acmode == NEW_COPY) {
	    # Check that we will not be clobbering an existing mask.
	    if (!fnullfile(Memc[fname]) && access (Memc[fname], 0, 0) == YES)
		if (envgetb ("imclobber")) {
		    iferr (call delete (Memc[fname]))
			;
		} else {
		    call pl_close (pl)
		    call imerr (IM_NAME(im), SYS_IKICLOB)
		}
	} else {
	    # Load the named mask if opening an existing mask image.
	    iferr (call pl_loadf (pl,Memc[fname],Memc[hp],IM_LENHDRMEM(im))) {
		call pl_close (pl)
		call sfree (sp)
		status = ERR
		return
	    }

	    # Set the image size.
	    call pl_gsize (pl, naxes, axlen, depth)

	    IM_NDIM(im) = naxes
	    call amovl (axlen, IM_LEN(im,1), IM_MAXDIM)
	    call imioff (im, 1, YES, 1)

	    # Restore the header cards.
	    call im_pmldhdr (im, hp)
	}

	# More set up of the image descriptor.
	IM_PL(im)	= pl
	IM_PLFLAGS(im)	= PL_CLOSEPL
	IM_PIXTYPE(im)  = TY_INT

	status = OK
	call sfree (sp)
end
