# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>
include	<pmset.h>
include	<plio.h>


# PLF_OPEN -- Open a PMIO mask on an image descriptor.

procedure plf_open (im, o_im,
	root, extn, ksection, cl_index, cl_size, acmode, status)

pointer	im			#I image descriptor
pointer	o_im			#I [not used]
char	root[ARB]		#I root image name
char	extn[ARB]		#I filename extension
char	ksection[ARB]		#I QPIO filter expression
int	cl_index		#I [not used]
int	cl_size			#I [not used]
int	acmode			#I [not used]
int	status			#O ok|err

int	naxes, axlen[IM_MAXDIM], depth
pointer	sp, fname, hp, pl
pointer	pl_open()
bool	envgetb()
int	access()
errchk	imerr

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (hp, IM_LENHDRMEM(im), TY_CHAR)

	# Get mask file name.
	call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)
	call strcpy (Memc[fname], IM_NAME(im), SZ_IMNAME)

	# Open an empty mask.
	pl = pl_open (NULL)

	if (acmode == NEW_IMAGE || acmode == NEW_COPY) {
	    # Check that we will not be clobbering an existing mask.
	    if (access (Memc[fname], 0, 0) == YES)
		if (!envgetb ("clobber")) {
		    call pl_close (pl)
		    call imerr (IM_NAME(im), SYS_FCLOBBER)
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
