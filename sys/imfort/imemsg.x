# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	"imfort.h"

define	SZ_OPNAME	64

# IMEMSG -- Translate an IMFORT or VOS error code into a message string.

procedure imemsg (ier, errmsg)

int	ier			# error code
%	character*(*) errmsg

pointer	sp, ostr
int	e_ier
char	e_opname[SZ_OPNAME]
common	/imemcm/ e_ier, e_opname

begin
	switch (ier) {
	case IE_ACCPIX:
%	    errmsg = 'error writing into pixel file during image create'
	case IE_ALCPIX:
%	    errmsg = 'cannot create or allocate space for pixel file'
	case IE_CLSHDR:
%	    errmsg = 'error closing image header file'
	case IE_CLSPIX:
%	    errmsg = 'error closing image pixel file'
	case IE_CREHDR:
%	    errmsg = 'cannot create image'
	case IE_FLUSH:
%	    errmsg = 'error flushing buffered data to pixel file'
	case IE_GCMDLN:
%	    errmsg = 'cannot read command line string'
	case IE_IMDELETE:
%	    errmsg = 'cannot delete image'
	case IE_IMDELNEXIM:
%	    errmsg = 'attempt to delete a nonexistent image'
	case IE_IMRENAME:
%	    errmsg = 'cannot rename image'
	case IE_IMRNAMNEXIM:
%	    errmsg = 'attempt to rename a nonexistent image'
	case IE_MAGIC:
%	    errmsg = 'illegal imfort image descriptor'
	case IE_NEXARG:
%	    errmsg = 'nonexistent command line argument referenced'
	case IE_NEXKW:
%	    errmsg = 'nonexistent header keyword referenced'
	case IE_NONNUMARG:
%	    errmsg = 'cannot decode numeric argument'
	case IE_NOTIMH:
%	    errmsg = 'attempt to access a non-image file as an image'
	case IE_NOTSHORT:
%	    errmsg = 'image is not of type short'
	case IE_OPEN:
%	    errmsg = 'cannot open image'
	case IE_OPNPIX:
%	    errmsg = 'cannot open pixel file'
	case IE_PIXTYPE:
%	    errmsg = 'image pixel type must be short or real'
	case IE_RDPIX:
%	    errmsg = 'error reading image pixel file'
	case IE_UPDHDR:
%	    errmsg = 'error updating image header file'
	case IE_UPDRO:
%	    errmsg = 'image header modified but image opened read only'
	case IE_WRHDR:
%	    errmsg = 'error writing to image header file'
	case IE_WRPIX:
%	    errmsg = 'error writing image pixel file'
	case IE_XOOB:
%	    errmsg = 'image x coordinates out of range or out of order'
	case IE_YOOB:
%	    errmsg = 'image y coordinates out of range'
	case IE_ZOOB:
%	    errmsg = 'image z coordinates out of range'
	case IE_EOF:
%	    errmsg = 'end of file or list detected'
	case IE_NAXIS:
%	    errmsg = 'wrong number of axes on image'
	case IE_AXLEN:
%	    errmsg = 'length of each image axis must be .ge. 1'
	case IE_MKDIR:
%	    errmsg = 'cannot create pixel subdirectory'
	case IE_PFNNUNIQ:
%	    errmsg = 'cannot create unique pixel file name'
	case IE_CLOBBER:
%	    errmsg = 'new image would overwrite existing image'

	case SYS_IDBDELNXKW:
%	    errmsg = 'attempt to delete unknown header keyword'
	case SYS_IDBKEYNF:
%	    errmsg = 'image header keyword not found'
	case SYS_IDBNODEL:
%	    errmsg = 'cannot delete image header keyword'
	case SYS_IDBOVFL:
%	    errmsg = 'out of space in image header'
	case SYS_IDBREDEF:
%	    errmsg = 'attempt to redefine an image header keyword'
	case SYS_IDBTYPE:
%	    errmsg = 'illegal header parameter data type conversion'
	case SYS_IMFNOVFL:
%	    errmsg = 'out of space for header keyword name list'

	default:
%	    errmsg = 'imfort error (unrecognized error code)'
	}

	# If the current error code agrees with that of the most recently
	# posted operand name, add the operand name to the error string.

	if (ier == e_ier && e_opname[1] != EOS) {
	    call smark (sp)
	    call salloc (ostr, SZ_LINE, TY_CHAR)

	    call f77upk (errmsg, Memc[ostr], SZ_LINE)
	    call strcat (" (", Memc[ostr], SZ_LINE)
	    call strcat (e_opname, Memc[ostr], SZ_LINE)
	    call strcat (")", Memc[ostr], SZ_LINE)
	    call f77pak (Memc[ostr], errmsg, len(errmsg))

	    call sfree (sp)
	}
end


# IM_SETERROP -- Called to set the operand name when an error occurs, so that
# it may be included in the error message string without being passed back to
# the user program.

procedure im_seterrop (ier, opname)

int	ier			# current error code
char	opname[ARB]		# associated operand name

int	e_ier
char	e_opname[SZ_OPNAME]
common	/imemcm/ e_ier, e_opname

begin
	e_ier = ier
	call strcpy (opname, e_opname, SZ_OPNAME)
end


# IM_SETERRIM -- A variation on im_seterrop, used to set the image name as
# the error operand, given the image descriptor.

procedure im_seterrim (ier, im)

int	ier			# current error code
pointer	im			# image descriptor

int	junk
pointer	sp, opname
int	fnroot()

begin
	call smark (sp)
	call salloc (opname, SZ_OPNAME, TY_CHAR)

	junk = fnroot (IM_HDRFILE(im), Memc[opname], TY_CHAR)
	call im_seterrop (ier, Memc[opname])

	call sfree (sp)
end
