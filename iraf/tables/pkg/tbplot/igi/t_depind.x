include <imhdr.h>

procedure t_depind ()

# DEPIND -- Prints to STDOUT the pixel values of two 1-D images.  This
# may be piped to graph to plot one vector against another.

pointer	sp
pointer	indvar, depvar		# Image names
pointer	ii, id			# Image descriptors
pointer	li, ld			# Pixel values
int	npts			# Minimum number of pixels

pointer	immap(), imgl1r()

begin
	call smark (sp)
	call salloc (indvar, SZ_FNAME, TY_CHAR)
	call salloc (depvar, SZ_FNAME, TY_CHAR)

	# Get the image names
	call clgstr ("indvar", Memc[indvar], SZ_FNAME)
	call clgstr ("depvar", Memc[depvar], SZ_FNAME)

	# Map the images
	ii = immap (Memc[indvar], READ_ONLY, 0)
	id = immap (Memc[depvar], READ_ONLY, 0)

	# Print only the smaller number of pixels
	# (Assume 1-D input)
	npts = min (IM_LEN(ii,1), IM_LEN(id,1))

	# Map the pixels
	li = imgl1r (ii)
	ld = imgl1r (id)

	# Print the values (use a procedure to "dereference" the memory
	call prtidp (Memr[li], Memr[ld], npts)

	# Close the images
	call imunmap (id)
	call imunmap (ii)

	call sfree (sp)
end


procedure prtidp (ind, dep, npts)

# PRTIDP -- Print a pair of pixel values per line from a pair of input
# vectors

real	ind[ARB], dep[ARB]	# Input vectors
int	npts			# Number of points to print

int	i

begin
	do i = 1, npts {
	    call printf ("%g %g\n")
		call pargr (ind[i])
		call pargr (dep[i])
	}
end
