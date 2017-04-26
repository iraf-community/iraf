# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

procedure preal (tval, rval)

char	tval[ARB]
real	rval

begin
	call eprintf ("%s %.4f\n")
	    call pargstr (tval)
	    call pargr (rval)
end
