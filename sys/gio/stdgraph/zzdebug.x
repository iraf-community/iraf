# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task	slio = t_slio

procedure t_slio()

pointer	gp
char	lbuf[SZ_LINE]
real	x[5], y[5]

pointer	gopen()
int	getline()

begin
	x[1] = .25;  y[1] = .25
	x[2] = .75;  y[2] = .25
	x[3] = .75;  y[3] = .75
	x[4] = .25;  y[4] = .75
	x[5] = .25;  y[5] = .25

	gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	call gpline (gp, x, y, 5)
	call gflush (gp)

	call putline (STDOUT, "enter text: ")
	call flush (STDOUT)

	if (getline (STDIN, lbuf) != EOF) {
	    call zwmsec (3000)
	    call printf ("text = %s")
		call pargstr (lbuf)
	    call flush (STDOUT)
	}

	call zwmsec (3000)
	call gclose (gp)
end
