include	<pkg/gtools.h>

# FLATTEN -- Flatten a spectrum and normalize to 1.0
# Use ICFIT for fitting the spectrum

procedure flatten (gp, gt, x, y, n)

pointer	gp, gt
real	x[n]
real	y[n]
int	n

bool	b
real	wx, z
int	i, key
pointer	sp, str, w, gt2, ic, cv

bool	clgetb()
real 	clgetr(), ic_getr(), cveval()
int	clgeti(), ic_geti(), btoi(), clgcur()
errchk	icg_fit

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (w, n, TY_REAL)

	key = '?'
	repeat {
	    switch (key) {
	    case '/', '-', 'f', 'c', 'n':
		call ic_open (ic)
		call clgstr ("function", Memc[str], SZ_FNAME)
		call ic_pstr (ic, "function", Memc[str])
		call ic_puti (ic, "order", clgeti ("order"))
		call ic_putr (ic, "low", clgetr ("low_reject"))
		call ic_putr (ic, "high", clgetr ("high_reject"))
		call ic_puti (ic, "niterate", clgeti ("niterate"))
		call ic_putr (ic, "grow", clgetr ("grow"))
		call ic_puti (ic, "markrej", btoi (clgetb ("markrej")))
		switch (key) {
		case '/':
		    call ic_puti (ic, "key", 4)
		case '-':
		    call ic_puti (ic, "key", 3)
		case 'f', 'n', 'c':
		    call ic_puti (ic, "key", 1)
		}

		call ic_putr (ic, "xmin", min (x[1], x[n]))
		call ic_putr (ic, "xmax", max (x[1], x[n]))

		call gt_copy (gt, gt2)
		call gt_gets (gt2, GTXLABEL, Memc[str], SZ_FNAME)
		call ic_pstr (ic, "xlabel", Memc[str])
		call gt_gets (gt2, GTYLABEL, Memc[str], SZ_FNAME)
		call ic_pstr (ic, "ylabel", Memc[str])
		call gt_gets (gt2, GTXUNITS, Memc[str], SZ_FNAME)
		call ic_pstr (ic, "xunits", Memc[str])
		call gt_gets (gt2, GTYUNITS, Memc[str], SZ_FNAME)
		call ic_pstr (ic, "yunits", Memc[str])

		call amovkr (1., Memr[w], n)
		call icg_fit (ic, gp, "cursor", gt2, cv, x, y, Memr[w], n)

		switch (key) {
		case '/':
		    do i = 1, n {
			z = cveval (cv, x[i])
			if (abs (z) < 1e-30)
			    y[i] = 1.
			else
			    y[i] = y[i] / z
		    }
		case '-':
		    do i = 1, n
			y[i] = y[i] - cveval (cv, x[i])
		case 'f':
		    do i = 1, n
			y[i] = cveval (cv, x[i])
		case 'c':
		    call ic_clean (ic, cv, x, y, Memr[w], n)
		case 'n':
		    ;
		}

		call ic_gstr (ic, "function", Memc[str], SZ_FNAME)
		call clpstr ("function", Memc[str])
		call clputi ("order", ic_geti (ic, "order"))
		call clputr ("low_reject", ic_getr (ic, "low"))
		call clputr ("high_reject", ic_getr (ic, "high"))
		call clputi ("niterate", ic_geti (ic, "niterate"))
		call clputr ("grow", ic_getr (ic, "grow"))
		b = (ic_geti (ic, "markrej") == YES)
		call clputb ("markrej", b)

		call cv_free (cv)
		call gt_free (gt2)
		call ic_closer (ic)
		break
	    case 'q':
		break
	    default:
		call printf (
		    "/=normalize, -=subtract, f=fit, c=clean, n=nop, q=quit")
	    }
	} until (clgcur ("cursor", wx, z, i, key, Memc[str], SZ_FNAME) == EOF)

	call sfree (sp)
end
