# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

.help imgnkw
.nf --------------------------------------------------------------------------
IMGNKW.X -- Header keyword list package.  A template is used to select some
subset of the header keywords, then successive elements are read from the list
in sequence until the end of the list is reached.

	imokwl (im, template, sortflag, kwl, ier)
	imgnkw (kwl, kwname, ier)
	imckwl (kwl, ier)

Standard IRAF pattern matching is used in the template: `*' matches all header
keywords, including the standard fields ("i_" prefix).
.endhelp ---------------------------------------------------------------------

# IMOKWL -- Open the keyword list.

procedure imokwl (im, patstr, sortit, kwl, ier)

pointer	im				# imfort image descriptor
%	character*(*) patstr
bool	sortit				# sort the list?
pointer	kwl				# receives list handle
int	ier

pointer	sp, pp
int	errcode()
pointer	imofnls(), imofnlu()

begin
	call smark (sp)
	call salloc (pp, SZ_LINE, TY_CHAR)

	call f77upk (patstr, Memc[pp], SZ_LINE)
	iferr {
	    if (sortit)
		kwl = imofnls (im, Memc[pp])
	    else
		kwl = imofnlu (im, Memc[pp])
	} then {
	    ier = errcode()
	} else
	    ier = OK

	call sfree (sp)
end


# IMGNKW -- Return the next keyword from the list.

procedure imgnkw (kwl, outstr, ier)

pointer	kwl				# image descriptor
%	character*(*) outstr
int	ier

int	nchars
pointer	sp, kp, ip
pointer	imgnfn()
int	errcode(), strncmp()

begin
	call smark (sp)
	call salloc (kp, SZ_FNAME, TY_CHAR)

	iferr (nchars = imgnfn (kwl, Memc[kp], SZ_FNAME)) {
	    ier = errcode()
	} else if (nchars == EOF) {
	    call f77pak ("END", outstr, len(outstr))
	    ier = IE_EOF
	} else {
	    ip = kp
	    if (strncmp (Memc[kp], "i_", 2) == 0)
		ip = ip + 2
	    call f77pak (Memc[ip], outstr, len(outstr))
	    ier = OK
	}

	call sfree (sp)
end


# IMCKWL -- Close the keyword list.

procedure imckwl (kwl, ier)

pointer	kwl			# image descriptor
int	ier
int	errcode()

begin
	iferr (call imcfnl (kwl))
	    ier = errcode()
	else
	    ier = OK
end
