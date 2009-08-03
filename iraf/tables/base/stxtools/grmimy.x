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
size_t	sz_val
long	l_val
long    v1[IM_MAXDIM], v2[IM_MAXDIM]    # Line and section counters.

long	junk                            # Generic.
size_t	npix                            # Length of a line of data.

pointer buf1, buf2                      # Data buffers.

# Function Prototypes.
long	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
long	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()

begin
        
	# Setup start vector for sequential reads and writes.
	l_val = 1
	sz_val = IM_MAXDIM
	call amovkl (l_val, v1, sz_val)
	call amovkl (l_val, v2, sz_val)
        
	# Copy the image.
	npix = IM_LEN(in, 1)
	switch (IM_PIXTYPE(in)) {
	case TY_SHORT:
	    while (imgnls (in, buf1, v1) != EOF) {
		junk = impnls (out, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
	    }
	case TY_USHORT, TY_INT:
	    while (imgnli (in, buf1, v1) != EOF) {
		junk = impnli (out, buf2, v2)
		call amovi (Memi[buf1], Memi[buf2], npix)
	    }
	case TY_LONG:
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
