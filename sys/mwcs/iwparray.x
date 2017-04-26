# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imwcs.h"

# IW_PUTARRAY -- Output a double floating array as a sequence of FITS cards,
# one value per card in the form "keyword = value", using the format string
# given to format the name of the FITS keyword.

procedure iw_putarray (iw, new, old, ndim, kw_format, kw_type, kw_index)

pointer	iw			#I pointer to IMWCS descriptor
double	new[ndim]		#I new array values
double	old[ndim]		#I old array values from header
int	ndim			#I image and WCS dimension
char	kw_format[ARB]		#I format for encoding keyword name
int	kw_type			#I IMWCS keyword type code
int	kw_index		#I keword index or 0 if don't care

int	axis
pointer	cp, im
char	kwname[SZ_KWNAME]
bool	fp_equald()
pointer	iw_findcard()
errchk	imaddf, imputd

begin
	do axis = 1, ndim {
	    # If new value is zero, no output, delete old card if present.
	    if (fp_equald (new[axis], 0.0D0))
		next

	    # See if we read the card for this parameter.
	    cp = iw_findcard (iw, kw_type, axis, kw_index)
	    im = IW_IM(iw)

	    # If value is unchanged, no need to do anything.
	    if (fp_equald (new[axis], old[axis])) {
		if (cp != NULL)
		    C_UPDATED(cp) = YES
		next
	    }

	    # Update the keyword in the image header.
	    call sprintf (kwname, SZ_KWNAME, kw_format)
		call pargi (axis)

	    if (cp == NULL)
		call imaddf (im, kwname, "d")
	    call imputd (im, kwname, new[axis])
	    if (cp != NULL)
		C_UPDATED(cp) = YES
	}
end
