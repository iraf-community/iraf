# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<plset.h>

# PL_ASCIIDUMP -- Dump a two dimensional region of a mask as a printable ASCII
# character array on the output stream.  Intended as a simple debugging tool;
# see also PL_SAVEIM.

procedure pl_asciidump (pl, vs, ve, outfd)

pointer	pl			#I mask descriptor
long	vs[ARB]			#I ll vector (only first two elements used)
long	ve[ARB]			#I ur vector (only first two elements used)
int	outfd			#I output file

size_t	sz_val
pointer	sp, pv, cv
size_t	npix
long	i, c_10
int	ch
long	v[PL_MAXDIM]
int	modi()
long	modl()
errchk	pl_valid

begin
	c_10 = 10

	call pl_valid (pl)
	npix = ve[1] - vs[1] + 1

	call smark (sp)
	call salloc (pv, npix, TY_INT)
	call salloc (cv, npix, TY_CHAR)

	# Output mask.
	sz_val = PL_MAXDIM
	call amovl (vs, v, sz_val)
	v[2] = ve[2]

	while (v[2] >= vs[2]) {
	    call pl_glpi (pl, v, Memi[pv], 0, npix, PIX_SRC)
	    do i = 1, npix {
		ch = modi (Memi[pv+i-1], 128)
		if (ch < 32)
		    ch = ch + 32
		if (ch <= 32 || ch == 127)
		    ch = '.'
		Memc[cv+i-1] = ch
	    }
	    call fprintf (outfd, "%3d ")
		call pargl (v[2])
	    call write (outfd, Memc[cv], npix)
	    call putci (outfd, '\n')
	    v[2] = v[2] - 1
	}

	# Label the columns.
	call fprintf (outfd, "%5t")
	do i = 1, npix {
	    ch = modl (i, c_10)
	    call putci (outfd, TO_DIGIT(ch))
	}
	call fprintf (outfd, "\n")

	call fprintf (outfd, "%5t")
	do i = 1, npix {
	    if (modl (i, c_10) == 0) {
		ch = i / 10
		call putci (outfd, TO_DIGIT(ch))
	    } else {
		call putci (outfd, ' ')
	    }
	}
	call fprintf (outfd, "\n")

	call sfree (sp)
end
