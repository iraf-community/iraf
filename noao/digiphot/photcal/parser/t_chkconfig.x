include	"../lib/parser.h"


# T_CHKCONFIG - Check the configuration file for syntax and semantic errors,
# and print information on the standard output about all the entities declared
# in it.

procedure t_chkconfig ()

char	input[SZ_FNAME]		# input file name
bool	verbose			# verbose output

bool	done
int	i, j, n, sym

bool	clgetb()
int	pr_parse(), pr_gsym(), pr_geti(), pr_gsymi(), pr_gpari()
real	pr_gsymr()
pointer	pr_xgetname(), pr_gsymc(), pr_gderc(), pr_gderp()

begin
	# Get the task parameters.
	call clgstr ("config", input, SZ_FNAME)
	verbose = clgetb ("verbose")

	# Print the beginning of compilation message.
	call printf ("\n\n** Beginning of compilation **\n\n")

	# Parse the input file.
	done = (pr_parse (input) == OK)

	# Print the end of compilation message.
	call printf ("\n** End of compilation **\n\n")

	# Print verbose output.
	if (done && verbose) {

	    # Print the catalog variables.
	    n = pr_geti (NCATVARS)
	    if (n > 0) {

		# Print section title
	        call printf (
		    "\nCATALOG VARIABLES, COLUMNS, AND ERROR COLUMNS:\n\n")

		# Loop over all variables
		do i = 1, n {
		    sym = pr_gsym (i, PTY_CATVAR)
		    if (pr_gsymi (sym, PINPSPARE) == YES)
			next
		    call printf ("%2d  %s\t%d\t%d\n")
			call pargi (pr_gsymi (sym, PSYMNUM))
			call pargstr (Memc[pr_xgetname (sym)])
			call pargi (pr_gsymi (sym, PINPCOL))
			call pargi (pr_gsymi (sym, PINPERRCOL))
		}
	    }

	    # Print input obervation variables.
	    n = pr_geti (NOBSVARS)
	    if (n > 0) {

		# Print section title
		call printf (
		   "\nOBSERVATIONAL VARIABLES, COLUMNS, AND ERROR COLUMNS:\n\n")

		# Loop over all variables
		do i = 1, n {
		    sym = pr_gsym (i, PTY_OBSVAR)
		    if (pr_gsymi (sym, PINPSPARE) == YES)
			next
		    call printf ("%2d  %s\t%d\t%d\n")
			call pargi (pr_gsymi (sym, PSYMNUM))
			call pargstr (Memc[pr_xgetname (sym)])
			call pargi (pr_gsymi (sym, PINPCOL))
			call pargi (pr_gsymi (sym, PINPERRCOL))
		}
	    }

	    # Print the fitting and constant parameters.
	    n = pr_geti (NTOTPARS)
	    if (n > 0) {

		# Print section title
		call printf (
		    "\nFIT AND CONSTANT PARAMETER VALUES:\n\n")

		# Loop over all fitting parameters
		do i = 1, n {
		    sym = pr_gsym (i, PTY_FITPAR)
		    call printf ("%2d  %s\t%g\t%s\n")
			call pargi (i)
			call pargstr (Memc[pr_xgetname (sym)])
			call pargr (pr_gsymr (sym, PFITVALUE))
			if (pr_gsymi (sym, PSYMTYPE) == PTY_CONST)
			    call pargstr ("(constant)")
			else
			    call pargstr ("")
		}
	    }

	    # Print the set equations.
	    n = pr_geti (NSETEQS)
	    if (n > 0) {

		# Print title
		call printf (
		    "\nAUXILIARY (SET) EQUATIONS:\n\n")

		# Loop over all equations
		do i = 1, n {

		    # Print the equation.
		    sym = pr_gsym (i, PTY_SETEQ)
		    call printf ("%2d %s = %s\n")
			call pargi (pr_gsymi (sym, PSYMNUM))
			call pargstr (Memc[pr_xgetname (sym)])
			call pargstr (Memc[pr_gsymc (sym, PSEQEQ)])

		    # Print the error equation.
		    call printf ("   error = %s,  min = %s,  max = %s\n")
			call pargstr (Memc[pr_gsymc (sym, PSEQERROR)])
			call pargstr (Memc[pr_gsymc (sym, PSEQERRMIN)])
			call pargstr (Memc[pr_gsymc (sym, PSEQERRMAX)])

#		    # Print the weight equation.
#		    call printf ("   weight = %s,  min = %s,  max = %s\n")
#			call pargstr (Memc[pr_gsymc (sym, PSEQWEIGHT)])
#			call pargstr (Memc[pr_gsymc (sym, PSEQWTSMIN)])
#			call pargstr (Memc[pr_gsymc (sym, PSEQWTSMAX)])

		    call printf ("\n")
		}
	    }

	    # Print the transformation equations.
	    n = pr_geti (NTRNEQS)
	    if (n > 0) {

		# Print section title
		call printf (
		    "\nTRANSFORMATION EQUATIONS:\n\n")

		# Loop over all equations
		do i = 1, n {

		    # Print the equation.
		    sym = pr_gsym (i, PTY_TRNEQ)
		    call printf ("%2d %s:  %s = %s\n")
			call pargi (pr_gsymi (sym, PSYMNUM))
			call pargstr (Memc[pr_xgetname (sym)])
			call pargstr (Memc[pr_gsymc (sym, PTEQREF)])
			call pargstr (Memc[pr_gsymc (sym, PTEQFIT)])

		    # Print the derivative equations.
		    do j = 1, pr_gsymi (sym, PTEQNPAR) {
			if (pr_gderp (sym, j, PTEQRPNDER) != NULL) {
			    call printf ("   derivative (%s, %s) = %s\n")
			        call pargstr (Memc[pr_xgetname (sym)])
			        call pargstr (Memc[pr_xgetname (pr_gpari (sym,
				    j, PTEQPAR))])
			        call pargstr (Memc[pr_gderc (sym, j, PTEQDER)])
			} else {
			    call printf ("   delta(%s, %s) = %s\n")
			        call pargstr (Memc[pr_xgetname (sym)])
			        call pargstr (Memc[pr_xgetname (pr_gpari (sym,
				    j, PTEQPAR))])
				call pargr (pr_gsymr(pr_gpari (sym, j, PTEQPAR),
				    PFITDELTA))
			}
		    }

		    # Print the error equation.
		    call printf ("   error = %s,  min = %s,  max = %s\n")
			call pargstr (Memc[pr_gsymc (sym, PTEQERROR)])
			call pargstr (Memc[pr_gsymc (sym, PTEQERRMIN)])
			call pargstr (Memc[pr_gsymc (sym, PTEQERRMAX)])

		    # Print the weight equation.
		    call printf ("   weight = %s,  min = %s,  max = %s\n")
			call pargstr (Memc[pr_gsymc (sym, PTEQWEIGHT)])
			call pargstr (Memc[pr_gsymc (sym, PTEQWTSMIN)])
			call pargstr (Memc[pr_gsymc (sym, PTEQWTSMAX)])

		    # Print the plot defaults.
		    call printf ("   plot  x = %s,  y = %s\n")
			call pargstr (Memc[pr_gsymc (sym, PTEQXPLOT)])
			call pargstr (Memc[pr_gsymc (sym, PTEQYPLOT)])

		    call printf ("\n")
		}
		call printf ("\n")
	    }
	}

	# Print the counter values.
	call printf ("Catalog input variables         = %d\n")
	    call pargi (pr_geti (NCATVARS))
	call printf ("First catalog column            = %d\n")
	    call pargi (pr_geti (MINCATCOL))
	call printf ("Last catalog column             = %d\n\n")
	    call pargi (pr_geti (MAXCATCOL))
	call printf ("Observational input variables   = %d\n")
	    call pargi (pr_geti (NOBSVARS))
	call printf ("First observational column      = %d\n")
	    call pargi (pr_geti (MINOBSCOL))
	call printf ("Last observational column       = %d\n\n")
	    call pargi (pr_geti (MAXOBSCOL))
	call printf ("Fitting parameters              = %d\n")
	    call pargi (pr_geti (NFITPARS))
	call printf ("Constant parameters             = %d\n\n")
	    call pargi (pr_geti (NTOTPARS) - pr_geti (NFITPARS))
#	call printf ("Extinction equations            = %d\n")
#	    call pargi (pr_geti (NEXTEQS))
	call printf ("Auxiliary (set) equations       = %d\n")
	    call pargi (pr_geti (NSETEQS))
	call printf ("Transformation equations        = %d\n\n")
	    call pargi (pr_geti (NTRNEQS))
	call printf ("Warnings                        = %d\n")
	    call pargi (pr_geti (NWARNINGS))
	call printf ("Errors                          = %d\n")
	    call pargi (pr_geti (NERRORS))
	call flush (STDOUT)

	# Free the tables.
	if (done)
	    call pr_free ()
end
