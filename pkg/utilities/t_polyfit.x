# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>

define	POLY_INSTRUMENTAL	1
define	POLY_UNIFORM		2
define	POLY_STATISTICAL	3

define	MAX_ITEMS	1000		# maximum number of data elements

# POLYFIT -- Fit a polynomial to the list of input pairs (x,y, sigmay)
# A polynomial fit of user specifiable order is made to the data.
#
#    y = a0 + a1*x + a2*x**2 + ...
#
# The values for the coeficients a0 - aN are printed on the
# first line of the standard output. The uncertainties in the
# coeficients are then printed on the next line.
#
# Optionally (verbose = yes) the values for chi-square, ftest,
# and correlation coefficient are printed along with the calculated
# values from the fit for the dependent variable.
#
# If listdata = yes, then the only output will be pairs of
# X,Yc values. Yc is the value of dependent variable as Calculated
# from the fit. This option allows piping to the GRAPH task.
#
# The data are taken from STDIN, a file, or a list of files.
# In the latter case, each data file results in an independent
# set of results.

# The routines REGRES and MATINV from Bevington are used to perfrom
# the fit.

procedure t_polyfit()

char	fname[SZ_FNAME], weights[SZ_FNAME]
bool	verbose, listdata
int	filelist, order, weighting
int	clpopni(), clgfil(), clgeti(), clgwrd()
bool	clgetb()
define	exit_ 91

begin
	# Input can come from the standard input, a file, or a list of files.
	# The following procedure makes both cases look like a list of files.

	filelist = clpopni ("input")
	order    = clgeti  ("order")
	weighting = clgwrd ("weighting", weights, SZ_FNAME,
	    ",instrumental,uniform,statistical,")
	verbose  = clgetb  ("verbose")
	listdata = clgetb  ("listdata")

	while (clgfil (filelist, fname, SZ_FNAME) != EOF)
	    call pf_fitdatalist (fname, order, weighting, verbose, listdata)

	call clpcls (filelist)
end


# PF_FITDATALIST -- Perform polynomial fit to data from named file.

procedure pf_fitdatalist (listfile, order, weighting, verbose, listdata)

char	listfile[SZ_FNAME]			# input file
int	order					# polynomial order
int	weighting				# mode of weighting fit
bool	verbose, listdata			# printout options

int	i, in, item, m[50], mode, nterms, line_number
real	x[MAX_ITEMS], y[MAX_ITEMS], yfit[MAX_ITEMS], sigy[MAX_ITEMS]
real	a0, a[50], siga0, siga[50], r[50], rmul, chisqr, ftest
real	stdev

extern	pf_fctn()
bool	fp_equalr()
int	fscan(), nscan(), open()
errchk	open, fscan, printf

begin
	# Set term selection array for REGRES.
	for (i=1;  i <= 50;  i=i+1)
	    m[i] = i

	in = open (listfile, READ_ONLY, TEXT_FILE)

	# Read successive X,Y, SIGMAY triples from the standard input,
	# accumulating the values in the arrays X, Y, weight.  Skip list
	# elements containing less than two numbers. The maximum number of
	# elements that can be read is fixed.

	item   = 1
	line_number = 0

	while ((fscan (in) != EOF) && (item < MAX_ITEMS+1)) {
	    line_number = line_number + 1

	    call gargr (x[item])
	    call gargr (y[item])

	    # There must be two items per entry for the x,y pair.
	    if (nscan() < 2) {
		call eprintf ("Bad entry in list on line:%d - item ignored\n")
		    call pargi (line_number)
		next
	    }

	    # Set undefined errors to 0.0.
	    if (weighting ==  POLY_INSTRUMENTAL) {
	        call gargr (sigy[item])
	        if (nscan() < 3) {
		    call eprintf ("Undefined sigmay on line:%d - item ignored\n")
			call pargi (line_number)
		    next
		} else if (fp_equalr (sigy[item], 0.0)) {
		    call eprintf ("Zero-valued sigmay on line:%d - item ignored\n")
			call pargi (line_number)
		    next
		}
		    
	    } else
		sigy[item] = 0

	    item = item + 1
	}

	item = item - 1
	if (item > MAX_ITEMS) {
	     call printf ("Number of data elements exceeded - max=%d\n")
		call pargi (MAX_ITEMS)
	}

	if (item <= order) {
	    call eprintf ("Not enough data for fit: order=%d, items=%d\n")
		call pargi (order)
		call pargi (item)
	    goto exit_
	}

	# It is necessary to scale the dependent variable values
	# to 1.0 on average to minimize the dynamic range during
	# matrix inversion.

	nterms = order
	switch (weighting) {
	case POLY_INSTRUMENTAL:
	    mode = 1
	case POLY_UNIFORM:
	    mode   = 0
	case POLY_STATISTICAL:
	    mode = -1
	default:
	    mode = 0
	}

	call pf_regres (x, y, sigy, item, nterms, m, mode, yfit,
	    a0, a, siga0, siga, r, rmul, chisqr, ftest, pf_fctn)

	# Compute standard deviation of residuals from reduced chi-sqr.
	switch (weighting) {
	case POLY_STATISTICAL, POLY_INSTRUMENTAL:
	    stdev = 0.0
	    do i = 1, item
		stdev = stdev + (y[i] - yfit[i]) ** 2
	    stdev = sqrt (stdev / (item - 1))
	case POLY_UNIFORM:
	    stdev = sqrt ((item - nterms - 1) * chisqr / (item - 1))
	default:
	    stdev = sqrt ((item - nterms - 1) * chisqr / (item - 1))
	}

	# Print coefficients scaled back to input Y levels
	if (!listdata) {
	    call printf ("%12.7g")
		call pargr (a0)

	    for (i=1;  i <= order;  i=i+1) {
		call printf ("  %12.7g")
		    call pargr (a[i])
	    }
	    call printf ("\n")

	    # Print sigmas.
	    call printf ("%12.7g")
	        call pargr (siga0)

	    for (i=1;  i <= order;  i=i+1) {
	        call printf ("  %12.7g")
		    call pargr (siga[i])
	    }
	    call printf ("\n")

	    # If verbose option specified, also print chi-square, ftest
	    # correlation coefficient, standard deviation of residuals,
	    # number of input pairs, and calculated y-values.
	    # ***25Nov85,SeH - ftest is undefined when correlation = 1.

	    if (verbose) {

		if (fp_equalr (ftest, -99999.)) {
		    call printf ("\nchi sqr: %7g  ftest: UNDEF  ")
		        call pargr (chisqr)
		    if (fp_equalr (rmul, -99999.)) {
		        call printf (" correlation: UNDEF\n")
		            call pargr (rmul)
		    } else {
		        call printf (" correlation: %7g\n")
		            call pargr (rmul)
		    }
		} else {
		    call printf ("\nchi sqr: %7g   ftest: %7g   ")
		        call pargr (chisqr)
		        call pargr (ftest)
		    if (fp_equalr (rmul, -99999.)) {
		        call printf (" correlation: UNDEF\n")
		            call pargr (rmul)
		    } else {
		        call printf ("correlation: %7g\n")
		            call pargr (rmul)
		    }
		}

		call printf (" nr pts: %7g   std dev res: %0.6g\n")
		    call pargi (item)
		    call pargr (stdev)

		call printf ("\nx(data)     y(calc)     y(data)     sigy(data)\n")
	    }
	}

	if (verbose || listdata) {
	    for (i=1;  i <= item;  i=i+1) {
		call printf ("%7g     %7g     %7g     %7g\n")
		    call pargr (x[i])
		    call pargr (yfit[i])
		    call pargr (y[i])
		    call pargr (sigy[i])
	    }
	}

exit_
	call close (in)
end
