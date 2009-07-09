include	<imhdr.h>


define	SZ_REGION	4	# Size of region structure
define	C1	Meml[P2L($1)]	# Minimum column
define	C2	Meml[P2L($1+1)]	# Maximum column
define	L1	Meml[P2L($1+2)]	# Minimum line
define	L2	Meml[P2L($1+3)]	# Maximum line

# T_TEXT2MASK -- Create a text file description (FIXPIX) from a mask.

procedure t_mask2text ()

pointer	mask			# Pixel mask
pointer	text			# Text file

size_t	sz_val
int	i, fd, nalloc, nregions
long	nc, nl, c1, c2, l
pointer	sp, regions, p, pmatch, im, bp

pointer	immap(), imgl2s()
int	open()
errchk	immap, open
include	<nullptr.inc>

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (text, sz_val, TY_CHAR)
	call salloc (mask, sz_val, TY_CHAR)

	# Get task parameters.
	call clgstr ("mask", Memc[mask], SZ_FNAME)
	call clgstr ("text", Memc[text], SZ_FNAME)

	# Open the files and abort on an error.
	im = immap (Memc[mask], READ_ONLY, NULLPTR)
	fd = open (Memc[text], NEW_FILE, TEXT_FILE)

	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)

	nalloc = 0
	nregions = 0
	do l = 1, nl {
	    bp = imgl2s (im, l) - 1
	    for (c1=1; c1<=nc && Mems[bp+c1]==0; c1=c1+1)
		;
	    while (c1 <= nc) {
		for (c2=c1+1; c2<=nc && Mems[bp+c2]!=0; c2=c2+1)
		    ;
		c2 = c2 - 1
		pmatch = NULL
		for (i=0; i<nregions; i=i+1) {
		    p = Memp[regions+i]
		    if (c1 <= C2(p) && c2 >= C1(p)) {
			if (pmatch == NULL) {
			    L2(p) = l
			    C1(p) = min (C1(p), c1)
			    C2(p) = max (C2(p), c2)
			    pmatch = p
			} else {
			    L1(pmatch) = min (L1(pmatch), L1(p))
			    C1(pmatch) = min (C1(pmatch), C1(p))
			    C2(pmatch) = max (C2(pmatch), C2(p))
			    Memp[regions+i] = Memp[regions+nregions-1]
			    Memp[regions+nregions-1] = p
			    nregions = nregions - 1
			    i = i - 1
			}
		    }
		}
		if (pmatch == NULL) {
		    if (nregions == nalloc) {
			nalloc = nregions + 1
			if (nalloc == 1) {
			    sz_val = nalloc
			    call malloc (regions, sz_val, TY_POINTER)
			} else {
			    sz_val = nalloc
			    call realloc (regions, sz_val, TY_POINTER)
			}
			sz_val = SZ_REGION
			call salloc (Memp[regions+nregions], sz_val, TY_STRUCT)
		    }
		    p = Memp[regions+nregions]
		    L1(p) = l
		    L2(p) = l
		    C1(p) = c1
		    C2(p) = c2
		    nregions = nregions + 1
		}
		for (c1=c2+1; c1<=nc && Mems[bp+c1]==0; c1=c1+1)
		    ;
	    }
	    for (i=0; i<nregions; i=i+1) {
		p = Memp[regions+i]
		if (L2(p) != l) {
		    call fprintf (fd, "%4d %4d %4d %4d\n")
			call pargl (C1(p))
			call pargl (C2(p))
			call pargl (L1(p))
			call pargl (L2(p))
		    Memp[regions+i] = Memp[regions+nregions-1]
		    Memp[regions+nregions-1] = p
		    nregions = nregions - 1
		    i = i - 1
		}
	    }
	}
	for (i=0; i<nregions; i=i+1) {
	    p = Memp[regions+i]
	    call fprintf (fd, "%4d %4d %4d %4d\n")
		call pargl (C1(p))
		call pargl (C2(p))
		call pargl (L1(p))
		call pargl (L2(p))
	}

	call close (fd)
	call imunmap (im)
	call mfree (regions, TY_POINTER)
	call sfree (sp)
end
