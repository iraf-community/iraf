include <gset.h>

procedure rc ()

pointer	gp
real	wx, wy
int	wcs, key
char	strval[SZ_LINE]

pointer	gopen()
int	clgcur()

begin
	gp = gopen ("stdgraph", APPEND, STDGRAPH)

	while (clgcur ("coord", wx, wy, wcs, key, strval, SZ_LINE) != EOF) {
	    switch (key) {
	    case 'q':
		break
	    case ':':
		call gseti (gp, G_WCS, wcs)
		call gtext (gp, wx, wy, strval, EOS)
		call printf ("%s\n")
		    call pargstr (strval)
	    }
	}

	call gclose (gp)
end
