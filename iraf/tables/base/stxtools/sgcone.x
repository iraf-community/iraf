#---------------------------------------------------------------------------
.help sg_convolve Jun93 source
.ih
NAME
sg_convolve -- Convolve an array using Savitzky-Golay filter.
.ih
USAGE
call sg_convolve (size, order, in, out, n)
.ih
ARGUMENTS
.ls size (I: int)
The full size of the smoothing kernel.  If less than or equal to 1,
then no convolving takes place.
.le
.ls order (I: int)
The order of the smoothing polynomial.  For normal "boxcar" smoothing,
this should be 0 or 1.  Greater values preserve higher order terms in the
original data.  Larger sizes are needed for this to be effective.
.le
.ls in (I: double[n])
The data array to be convolved.
.le
.ls out (O: double[n])
The convolved array.  May be the same as the input array.
.le
.ih
DESCRIPTION
The routine, savgol, is used to calculate a Savitsky-Golay convolving
kernel.  This kernel is then applied, using standard routines, to the
input data.  See the routine savgol for more information.
.ih
SEE ALSO
savgol
.endhelp
#---------------------------------------------------------------------------
procedure sg_convolve (size, order, in, out, n)

int     size                    # I:  The size of the filter.
int     order                   # I:  The order to preserve while filtering.
double  in[n]                   # I:  Data to be convolved.
double  out[n]                  # O:  The convolved data.
int	n                       # I:  Length of the arrays.

# Kernel parameters.
size_t	sz_val, sz_val1
int	half                    # Half size of kernel.
int	isize                   # Odd size of kernel.
pointer k, kx                   # The kernel in real/double-wrap versions.

# Misc.
pointer adx                     # Generic double array.
int     i                       # Generic.
pointer sp                      # Stack pointer.

begin
        call smark (sp)

        # Fix the kernel size to be odd.
        half = size / 2
        isize = half * 2 + 1

        # Make sure there is something to convolve.  If not, just copy and
        # run.
        if (isize <= 1) {
	    sz_val = n
            call amovd (in, out, sz_val)
        } else {
            sz_val = isize
            call salloc (k, sz_val, TY_REAL)
            call salloc (kx, sz_val, TY_DOUBLE)
            sz_val = n+isize
            call salloc (adx, sz_val, TY_DOUBLE)

            # Compute the kernel.
            call savgol (Memd[kx], isize, half, half, 0, order)
            do i = 0, half
                Memr[k+i] = Memd[kx+half-i]
            do i = 0, half-1
                Memr[k+half+i+1] = Memd[kx+isize -i-1]

            # Put the data in the extended array and pad the ends as
            # constants.
	    sz_val = n
            call amovd (in, Memd[adx+half], sz_val)
            do i = 1, half {
                Memd[adx+half-i] = in[1]
                Memd[adx+half+n+i-1] = in[n]
            }

            # Filter it.
	    sz_val = n
	    sz_val1 = isize
            call acnvrd (Memd[adx], out, sz_val, Memr[k], sz_val1)
        }

        # That's all folks.
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of sg_convolve
#---------------------------------------------------------------------------
