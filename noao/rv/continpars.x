include "rvpackage.h"
include "rvflags.h"
include "rvcomdef.h"
include "rvcont.h"

.help continpars
.nf ___________________________________________________________________________
CONTINPARS  - Support routines for the 'continpars' named external pset.  

	This file include routines for opening/closing the contin structure 
as well as command handling.  Command handling is limited to changing the 
parameter values or resetting them to the default values.  Routines included
here are as follows.

	 	        cont_open (rv)
	               cont_close (rv)
	            cont_get_pars (rv, sample, func)
		   cont_parupdate (rv)
		     cont_unlearn (rv)
		        cont_show (rv)
		       cont_colon (rv, cmdstr)
		  cmd_interactive (rv)
		       cmd_sample (rv)
		     cmd_naverage (rv)
		     cmd_function (rv)
		      cmd_replace (rv)
		     cmd_cn_order (rv)
		   cmd_low_reject (rv)
		  cmd_high_reject (rv)
		     cmd_niterate (rv)
		         cmd_grow (rv)

	The 'cmd_' prefix indicates that the routine is called from a colon 
command to either print the current value or set the new value for that
field.  Other routines should be self-explanatory

.endhelp _____________________________________________________________________

# Default values for the CONTPARS pset
define	DEF_INTERACTIVE		NO		# Fit continuum interactively?
define	DEF_TYPE		DIFF		# Type of output(fit|diff|ratio)
define	DEF_SAMPLE		"*"		# Sample of points to use in fit
define	DEF_NAVERAGE		1		# Npts in sample averaging
define	DEF_FUNCTION		CN_SPLINE3	# Fitting function
define	DEF_ORDER		1		# Order of fitting function
define	DEF_REPLACE		NO		# Replace spec w/ fit?
define	DEF_LOW_REJECT		2.		# Low rejection in sigma of fit
define	DEF_HIGH_REJECT		2.		# High rejection in sigma of fit
define	DEF_NITERATE		10		# Number of rejection iterations
define	DEF_GROW		1.		# Rejection growing radius


# CONT_OPEN - Open the Process parameters substructure.  This is used to
# reduce the size of the already over-burdened main RV struct.

procedure cont_open (rv)

pointer	rv					#I RV struct pointer

pointer	cptr

begin
	iferr (call calloc (cptr, SZ_CONT_STRUCT, TY_STRUCT))
	    call error (0, "Error allocating sub-structure RV_CONT.")

	RV_CONT(rv) = cptr

	# Initlialize the values
	call calloc (CON_SAMPLE(rv), 2*SZ_LINE, TY_CHAR)
	call calloc (CON_FUNC(rv), SZ_FNAME, TY_CHAR)
	call cont_get_pars (rv, Memc[CON_SAMPLE(rv)], Memc[CON_FUNC(rv)])
end


# CONT_CLOSE - Close the continpars structure.

procedure cont_close (rv)

pointer	rv					#I RV struct pointer

begin
	call mfree (CON_FUNC(rv), TY_CHAR)
	call mfree (CON_SAMPLE(rv), TY_CHAR)
	call mfree (RV_CONT(rv), TY_STRUCT)
end


# CONT_GET_PARS - Get the continuum fitting parameters from the pset.

procedure cont_get_pars (rv, sample, func)

pointer	rv					#I RV struct pointer
char	sample[SZ_FNAME]			#U Sample points used
char	func[SZ_FNAME]				#U Function name for fit

pointer	pp
pointer	sp, rbf, bp, bp1, bp2

pointer	clopset()
int	strdic(), clgpseti(), btoi()
real	clgpsetr()
bool	clgpsetb(), streq()

begin
	# Get continuum parameters.
	iferr (pp = clopset("continpars"))
	    call error (0, "Error opening `continpars' pset")

	call smark (sp)
	call salloc (bp, SZ_LINE, TY_CHAR)
	call salloc (bp1, SZ_LINE, TY_CHAR)
	call salloc (bp2, SZ_LINE, TY_CHAR)
	call salloc (rbf, SZ_FNAME, TY_CHAR)

	call clgpset (pp, "c_function", func, SZ_LINE)
	if (streq(func,"") || streq(func," "))
	    call error (0,"Continpars.function specified as empty string.")

	call clgpset (pp, "c_sample", sample, SZ_LINE)
	if (streq(sample,"") || streq(sample," "))
	    call strcpy ("*", sample, SZ_FNAME)

	CON_ORDER(rv) = clgpseti (pp, "order")
	CON_NITERATE(rv) = clgpseti (pp, "niterate")
	CON_NAVERAGE(rv) = clgpseti (pp, "naverage")
	CON_GROW(rv) = clgpsetr (pp, "grow")
	CON_LOWREJECT(rv) = clgpsetr (pp, "low_reject")
	CON_HIGHREJECT(rv) = clgpsetr (pp, "high_reject")
	CON_INTERACTIVE(rv) = btoi (clgpsetb(pp, "c_interactive"))
	CON_REPLACE(rv) = btoi (clgpsetb(pp, "replace"))

	CON_CNFUNC(rv) = strdic (func, func, SZ_LINE, CN_INTERP_MODE)
	if (CON_CNFUNC(rv) == 0) 
	    call error (0, "Unknown fitting function type")

	CON_MARKREJ(rv) = YES

	call clcpset (pp)				# Close pset
	call sfree (sp)
end


# CONT_PARUPDATE - Update the pset with the current values of the struct.

procedure cont_parupdate (rv)

pointer	rv					#I RV struct pointer

pointer	sp, b0, b1, b2
pointer	pp, clopset()
bool 	itob()
errchk  clopset

begin
	# Update contin params
	iferr (pp = clopset ("continpars")) {
	    call rv_errmsg ("Error opening `continpars' pset.")
	    return
	}

	call smark (sp)
	call salloc (b0, SZ_LINE, TY_CHAR)
	call salloc (b1, SZ_LINE, TY_CHAR)
	call salloc (b2, SZ_LINE, TY_CHAR)

	call clppseti (pp, "order", CON_ORDER(rv))
	call clppseti (pp, "naverage", CON_NAVERAGE(rv))
	call clppseti (pp, "niterate", CON_NITERATE(rv))

	call clppsetr (pp, "low_reject", CON_LOWREJECT(rv))
	call clppsetr (pp, "high_reject", CON_HIGHREJECT(rv))
	call clppsetr (pp, "grow", CON_GROW(rv))

	call clppsetb (pp, "c_interactive", itob(CON_INTERACTIVE(rv)))
	call clppsetb (pp, "replace", itob(CON_REPLACE(rv)))

	call nam_cninterp (rv, Memc[b1])
	call clppset (pp, "c_function", Memc[b1])

	if (CON_SAMPLE(rv) != NULL)
	    call clppset (pp, "c_sample", Memc[CON_SAMPLE(rv)])

	call clcpset (pp)
	call sfree (sp)
end


# CONT_UNLEARN - Unlearn the pset and replace with the default values.

procedure cont_unlearn (rv)

pointer	rv					#I RV struct pointer

begin
	CON_CNFUNC(rv) = DEF_FUNCTION
	CON_ORDER(rv) = DEF_ORDER
	CON_LOWREJECT(rv) = DEF_LOW_REJECT
	CON_HIGHREJECT(rv) = DEF_HIGH_REJECT
	CON_NITERATE(rv) = DEF_NITERATE
	CON_REPLACE(rv) = DEF_NITERATE
	CON_GROW(rv) = DEF_GROW
	CON_NAVERAGE(rv) = DEF_NAVERAGE
	CON_INTERACTIVE(rv) = DEF_INTERACTIVE

	if (CON_SAMPLE(rv) != NULL)
	    call strcpy (DEF_SAMPLE, Memc[CON_SAMPLE(rv)], SZ_FNAME)
end


# CONT_SHOW - Show the current contin parameters

procedure cont_show (rv, fd)

pointer	rv			#I RV struct pointer
pointer	fd			#I output file descriptor

pointer	sp, str, str1
bool	itob()

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1,SZ_LINE, TY_CHAR)

	call fprintf (fd, "%21tProcesspars PSET Values\n")
	call fprintf (fd, "%21t-----------------------\n\n")

	# Print the continpars info
	call fprintf (fd, "CONTINUUM parameters:\n")

	call fprintf (fd, "c_interactive%15t= %b\n")
	    call pargb (itob(CON_INTERACTIVE(rv)))
	call fprintf (fd, "c_sample%15t= '%.10s'\n")
	    call pargstr (Memc[CON_SAMPLE(rv)])
	call fprintf (fd, "naverage%15t= %d\n")
	    call pargi (CON_NAVERAGE(rv))
	call fprintf (fd, "c_function%15t= '%.10s'\n")
	    call pargstr (Memc[CON_FUNC(rv)])
	call fprintf (fd, "order%15t= %d\n")
	    call pargi (CON_ORDER(rv))
	call fprintf (fd, "replace%15t= %d\n")
	    call pargb (itob(CON_REPLACE(rv)))
	call fprintf (fd, "low_reject%15t= %g\n")
	    call pargr (CON_LOWREJECT(rv))
	call fprintf (fd, "high_reject%15t= %g\n")
	    call pargr (CON_HIGHREJECT(rv))
	call fprintf (fd, "niterate%15t= %d \n")
	    call pargi (CON_NITERATE(rv))
	call fprintf (fd, "grow%15t= %g\n")
	    call pargr (CON_GROW(rv))

	call fprintf (fd, "\n\n")
	call sfree (sp)
end


# CONT_COLON -- Process the continpars task colon commands.

procedure cont_colon (rv, cmdstr)

pointer	rv				#I pointer to the RV structure
char	cmdstr[SZ_LINE]			#I command string

pointer	sp, cmd, buf
int	strdic()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack the keyword from the string and look it up in the
	# dictionary.  Switch on command and call the appropriate routines.
	switch (strdic(Memc[cmd], Memc[cmd], SZ_FNAME, CONT_KEYWORDS)) {
	case CNT_INTERACTIVE:
	    call cmd_interactive (rv)
	case CNT_SAMPLE:
	    call cmd_sample (rv)
	case CNT_NAVERAGE:
	    call cmd_naverage (rv)
	case CNT_FUNCTION:
	    call cmd_cnfunc (rv)
	case CNT_CN_ORDER:
	    call cmd_cn_order (rv)
	case CNT_REPLACE:
	    call cmd_replace (rv)
	case CNT_LOW_REJECT:
	    call cmd_low_reject (rv)
	case CNT_HIGH_REJECT:
	    call cmd_high_reject (rv)
	case CNT_NITERATE:
	    call cmd_niterate (rv)
	case CNT_GROW:
	    call cmd_grow (rv)
	default:
	    call rv_errmsg ("")
	}

	call sfree (sp)
end


# CMD_INTERACTIVE - Set/Show the interactive continuum subtraction flag.

procedure cmd_interactive (rv)

pointer	rv						#I RV struct pointer

int	nscan(), btoi()
bool	bval, itob()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    CON_INTERACTIVE(rv) = btoi (bval)
	} else {
	    call printf ("continpars.c_interactive = %b")
		call pargb (itob(CON_INTERACTIVE(rv)))
	}
end


# CMD_SAMPLE - Set/Show the sample regions for continuum fitting.

procedure cmd_sample (rv)

pointer	rv

pointer	sp, buf
bool	streq()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    if (streq(Memc[buf],"") || streq(Memc[buf]," "))
	        call error (0, "continpars.c_sample specified as empty string.")
	    call strcpy (Memc[buf+1], Memc[CON_SAMPLE(rv)], SZ_LINE)
	} else {
	    call printf ("continpars.c_sample = '%s'")
	        call pargstr (Memc[CON_SAMPLE(rv)])
	}

	call sfree (sp)
end


# CMD_NAVERAGE - Set/Show the number of points to average in the fit.

procedure cmd_naverage (rv)

pointer	rv						#I RV struct pointer

int	ival, nscan()

begin
	call gargi (ival)
	if (nscan() == 2) {
	    CON_NAVERAGE(rv) = ival
	} else {
	    call printf ("continpars.naverage = %d")
		call pargi (CON_NAVERAGE(rv))
	}
end


# CMD_CNFUNC - Set/Show the fitting function used.

procedure cmd_cnfunc (rv)

pointer	rv						#I RV struct pointer

pointer	sp, buf, bp
int	cod_cninterp()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (bp, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    CON_CNFUNC(rv) = cod_cninterp (Memc[buf+1])
	    
	} else {
	    call nam_cninterp (rv, Memc[bp])
	    call printf ("continpars.c_function = '%s'")
		call pargstr (Memc[bp])
	}

	call sfree (sp)
end


# CMD_CN_ORDER - Set/Show the order of the function fit.

procedure cmd_cn_order (rv)

pointer	rv						#I RV struct pointer

int	ival, nscan()

begin
	call gargi (ival)
	if (nscan() == 2) {
	    CON_ORDER(rv) = ival
	} else {
	    call printf ("continpars.order = %d")
		call pargi (CON_ORDER(rv))
	}
end


# CMD_REPLACE - Set/Show the replace continuum subtraction flag.

procedure cmd_replace (rv)

pointer rv                                              #I RV struct pointer

int     nscan(), btoi()
bool    bval, itob()

begin
        call gargb (bval)
        if (nscan() == 2) {
            CON_REPLACE(rv) = btoi (bval)
        } else {
            call printf ("continpars.replace = %b")
                call pargb (itob(CON_REPLACE(rv)))
        }
end


# CMD_LOW_REJECT - Set/Show the lower sigma rejection limit.

procedure cmd_low_reject (rv)

pointer	rv						#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    CON_LOWREJECT(rv) = rval
	} else {
	    call printf ("continpars.low_reject = %g")
		call pargr (CON_LOWREJECT(rv))
	}
end


# CMD_HIGH_REJECT - Set/Show the upper sigma rejection limit.

procedure cmd_high_reject (rv)

pointer	rv						#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    CON_HIGHREJECT(rv) = rval
	} else {
	    call printf ("continpars.high_reject = %g")
		call pargr (CON_HIGHREJECT(rv))
	}
end


# CMD_NITERATE - Set/Show the number of iterations in the fit.

procedure cmd_niterate (rv)

pointer	rv						#I RV struct pointer

int	ival, nscan()

begin
	call gargi (ival)
	if (nscan() == 2) {
	    CON_NITERATE(rv) = ival
	} else {
	    call printf ("continpars.niterate = %d")
		call pargi (CON_NITERATE(rv))
	}
end


# CMD_GROW - Set/Show the rejection growing radius.

procedure cmd_grow (rv)

pointer	rv						#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    CON_GROW(rv) = rval
	} else {
	    call printf ("continpars.grow = %g")
		call pargr (CON_GROW(rv))
	}
end
