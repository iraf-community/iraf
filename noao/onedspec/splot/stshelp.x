# STS_HELP -- Issue a help line

procedure sts_help ()

int	linenr, maxline

data	linenr /1/
data	maxline/5/

begin
	switch (linenr) {
	case 1:
	    call printf (
		"c=cursor pos  m=mean/snr  s=smooth  w=window  f=functions")

	case 2:
	    call printf ("n=>fnu  l=>flambda  p=>wavelth  $=>channels  ")
	    call printf (".=upshift  ,=dnshift  z=xexpand")

	case 3:
	    call printf ("o=overplot  d=deblend  -=subtr blend  t=flatten   ")
	    call printf ("a=autoexp  g=plot new sp")

	case 4:
	    call printf ("j=fix pt   x=fix line  b=zero base  r=replot  ")
	    call printf ("q=quit  ?=help /=linehelp")

	case 5:
	    call printf (
	"i=write sp  e,k,v=eq. width  h=eqw(1side)  y=plot_std  u=set_wave")
	}
	call flush (STDOUT)

	linenr = linenr + 1
	if (linenr > maxline)
	    linenr = 1
end
