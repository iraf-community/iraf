include <imhdr.h>

#---------------------------------------------------------------------------
.help grm_imcopy Oct92 source
.ih
NAME
grm_imcopy -- Copy images given their image descriptors.
.endhelp
#---------------------------------------------------------------------------
procedure grm_imcopy (in, out)

pointer in                      # I:  Input image descriptor of image to copy.
pointer out                     # I:  Output image descriptor of resultant image.

# Declarations.
long    v1[IM_MAXDIM], v2[IM_MAXDIM]    # Line and section counters.

int     junk                            # Generic.
int     npix                            # Length of a line of data.

pointer buf1, buf2                      # Data buffers.

# Function Prototypes.
int	imgnls(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnll(), impnlr(), impnld(), impnlx()

begin
        
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)
        
	# Copy the image.
	npix = IM_LEN(in, 1)
	switch (IM_PIXTYPE(in)) {
	case TY_SHORT:
	    while (imgnls (in, buf1, v1) != EOF) {
		junk = impnls (out, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
	    }
	case TY_USHORT, TY_INT, TY_LONG:
	    while (imgnll (in, buf1, v1) != EOF) {
		junk = impnll (out, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
	    }
	case TY_REAL:
	    while (imgnlr (in, buf1, v1) != EOF) {
		junk = impnlr (out, buf2, v2)
		call amovr (Memr[buf1], Memr[buf2], npix)
	    }
	case TY_DOUBLE:
	    while (imgnld (in, buf1, v1) != EOF) {
		junk = impnld (out, buf2, v2)
		call amovd (Memd[buf1], Memd[buf2], npix)
	    }
	case TY_COMPLEX:
	    while (imgnlx (in, buf1, v1) != EOF) {
	        junk = impnlx (out, buf2, v2)
		call amovx (Memx[buf1], Memx[buf2], npix)
	    }
	default:
	    call error (1, "unknown pixel datatype")
	}
        
end
#---------------------------------------------------------------------------
# End of grm_imcopy
#---------------------------------------------------------------------------
