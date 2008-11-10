# T_RUNMED -- Apply running median to a list of input images.

procedure t_runmed ()

pointer	input			# List of input images
pointer	output			# List of output images
int	window			# Filter window
pointer	masks			# List of output masks
pointer	inmaskkey		# Input mask keyword
pointer	outmaskkey		# Output mask keyword
pointer	outtype			# Output type
bool	exclude			# Exclude input image?
real	nclip			# Clipping factor
int	navg			# Number of values to average
pointer	scale			# Scale specification
bool	normscale		# Normalize the scales to the first input?
bool	outscale		# Scale the output?
real	blank			# Blank values
pointer	storetype		# Storage type
bool	verbose			# Verbose?

pointer	sp

int	clgeti()
bool	clgetb()
real	clgetr()
pointer	imtopenp()

begin
	call smark (sp)
	call salloc (inmaskkey, SZ_FNAME, TY_CHAR)
	call salloc (outmaskkey, SZ_FNAME, TY_CHAR)
	call salloc (outtype, SZ_FNAME, TY_CHAR)
	call salloc (scale, SZ_FNAME, TY_CHAR)
	call salloc (storetype, SZ_FNAME, TY_CHAR)

	input = imtopenp ("input")
	output = imtopenp ("output")
	window = clgeti ("window")
	masks = imtopenp ("masks")
	call clgstr ("inmaskkey", Memc[inmaskkey], SZ_FNAME)
	call clgstr ("outmaskkey", Memc[outmaskkey], SZ_FNAME)
	call clgstr ("outtype", Memc[outtype], SZ_FNAME)
	exclude = clgetb ("exclude")
	nclip = clgetr ("nclip")
	navg = clgeti ("navg")
	call clgstr ("scale", Memc[scale], SZ_FNAME)
	blank = clgetr ("blank")
	normscale = clgetb ("normscale")
	outscale = clgetb ("outscale")
	call clgstr ("storetype", Memc[storetype], SZ_FNAME)
	verbose = clgetb ("verbose")

	call runmed (input, output, window, masks, Memc[inmaskkey],
	    Memc[outmaskkey], Memc[outtype], exclude, nclip, navg, Memc[scale],
	    normscale, outscale, blank, Memc[storetype], verbose)

	call imtclose (masks)
	call imtclose (output)
	call imtclose (input)
	call sfree (sp)
end
