include	"quadgeom.h"

procedure new ()

char    input[SZ_FNAME]         #TI Input image name.
char    instrument[SZ_FNAME]    #TI Instrument translation file
 
pointer in, qg
int	xtrim1, xtrim2, ytrim1, ytrim2, xskip1, xskip2
 
int	clgeti
pointer immap()
 
begin
 
        # Open instrument file
        call clgstr    ("instrument",  instrument,  SZ_FNAME)
        call hdmopen   (instrument)
 
        # Open input image
        call clgstr ("input",  input,  SZ_FNAME)
        in = immap  (input, READ_ONLY, 0)

	xtrim1 = clgeti ("xtrim1")
	xtrim2 = clgeti ("xtrim2")
	ytrim1 = clgeti ("ytrim1")
	ytrim2 = clgeti ("ytrim2")
	xskip1 = clgeti ("xskip1")
	xskip2 = clgeti ("xskip2")
 
        # Set-up section translation
        call quadalloc (qg)
        call qghdr2  (in, qg)
	call qguser (qg, xtrim1, xtrim2, ytrim1, ytrim2, xskip1, xskip2)
        call quaddump  (qg)
 
        # Tidy up
        call imunmap (in)
        call quadfree (qg)
        call hdmclose ()
end

procedure old ()

char    input[SZ_FNAME]         #TI Input image name.
char    instrument[SZ_FNAME]    #TI Instrument translation file
 
pointer in, qg
 
pointer immap()
 
begin
 
        # Open instrument file
        call clgstr    ("instrument",  instrument,  SZ_FNAME)
        call hdmopen   (instrument)
 
        # Open input image
        call clgstr ("input",  input,  SZ_FNAME)
        in = immap  (input, READ_ONLY, 0)
 
        # Set-up section translation
        call quadalloc (qg)
        call quadgeom  (in, qg, "", "")
        call quaddump  (qg)
 
        # Tidy up
        call imunmap (in)
        call quadfree (qg)
        call hdmclose ()
end
