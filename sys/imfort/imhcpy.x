# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"

# IMHCPY -- Copy the non-pixel fields of an existing image header to a new
# image header.  Only fields not set by IMCREA are copied.

procedure imhcpy (o_im, n_im, ier)

pointer	o_im		# old image
pointer	n_im		# new image
int	ier

int	junk
pointer	sp, root, o_ua, n_ua
string	imhdr "imhdr"
int	fnroot()
bool	strne()

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)

	if (strne (IM_MAGIC(o_im), imhdr) || strne (IM_MAGIC(n_im), imhdr)) {
	    ier = IE_MAGIC
	    call sfree (sp)
	    return
	}

	o_ua = IM_USERAREA(o_im)
	n_ua = IM_USERAREA(n_im)

	# Copy the non-pixel fields.
	call strcpy (IM_TITLE(o_im), IM_TITLE(n_im), SZ_IMTITLE)
	call strcpy (IM_HISTORY(o_im), IM_HISTORY(n_im), SZ_IMHIST)
	call strcpy (Memc[o_ua], Memc[n_ua], ARB)

	# Record the inheritance in the history buffer.
	junk = fnroot (IM_HDRFILE(o_im), Memc[root], SZ_FNAME)
	call strcat ("New copy of ", IM_HISTORY(n_im), SZ_IMHIST)
	call strcat (Memc[root], IM_HISTORY(n_im), SZ_IMHIST)
	call strcat ("\n", IM_HISTORY(n_im), SZ_IMHIST)

	IM_UPDATE(n_im) = YES

	ier = OK
	call sfree (sp)
end
