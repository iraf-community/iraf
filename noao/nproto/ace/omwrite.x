include	<imhdr.h>
include	<pmset.h>
include	"ace.h"


procedure omwrite (pm, fname, omtype, refim, cat, catalog, objid, logfd)

pointer	pm		#I Pixel mask pointer to save
char	fname[ARB]	#I Filename
int	omtype		#I Type of mask values
pointer	refim		#I Reference image pointer
pointer	cat		#I Catalog pointer
char	catalog[ARB]	#I Catalog filename
char	objid[ARB]	#I Object ID string
int	logfd		#I Logfile

int	i, j, k, nc, nl, stridxs(), andi()
long	v[2]
pointer	sp, str, im, buf, immap(), impl2i()

errchk	immap

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Remove output only fields.
	call strcpy (fname, Memc[str], SZ_LINE)
	i = stridxs (",", fname)
	if (i > 0) {
	    Memc[str+i-1] = ']'
	    Memc[str+i] = EOS
	}

	if (logfd != NULL) {
	    call fprintf (logfd, "  Write object mask: %s\n")
		call pargstr (Memc[str])
	}

	im = immap (fname, NEW_COPY, refim)
	IM_PIXTYPE(im) = TY_INT

	nc = IM_LEN(refim,1)
	nl = IM_LEN(refim,2)

	v[1] = 1
	switch (omtype) {
	case OM_BOOL:
	    do i = 1, nl {
		v[2] = i
		buf = impl2i (im, i)
		call pmglpi (pm, v, Memi[buf], 0, nc, PIX_SRC)
		call aminki (Memi[buf], 1, Memi[buf], nc)
	    }
	case OM_ONUM:
	    do i = 1, nl {
		v[2] = i
		buf = impl2i (im, i)
		call pmglpi (pm, v, Memi[buf], 0, nc, PIX_SRC)
		do j = buf, buf+nc-1
		    Memi[j] = MNUM(Memi[j])
	    }
	case OM_COLORS:
	    do i = 1, nl {
		v[2] = i
		buf = impl2i (im, i)
		call pmglpi (pm, v, Memi[buf], 0, nc, PIX_SRC)
		do j = buf, buf+nc-1 {
		    k = MNUM(Memi[j])
		    if (k > 0) {
			if (k < NUMSTART)
			    k = 1
			else
			    k = mod (k, 8) + 2
		    }
		    Memi[j] = k
		}
	    }
	default:
	    do i = 1, nl {
		v[2] = i
		call pmglpi (pm, v, Memi[impl2i(im,i)], 0, nc, PIX_SRC)
	    }
	}

	iferr (call imdelf (im, "DATASEC"))
	    ;
	iferr (call imdelf (im, "TRIMSEC"))
	    ;
	if (catalog[1] != EOS)
	    call imastr (im, "CATALOG", catalog)
	if (objid[1] != EOS)
	    call imastr (im, "OBJID", objid)

	call imastr (refim, "OBJMASK", Memc[str])

	call imunmap (im)
end
