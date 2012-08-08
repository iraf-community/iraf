.help fteval
Fit evaluation procedures.

These procedures are called by the (I)NLFIT package to compute the fitting
function, its derivatives at different points, and the plotting equations
for all points.
Pointers to the previous equation codes are stored in the evaluator common
to speed up computations, since they are accessed for every data point.
A buffer is allocated to store the derivative code pointers, since the
number of derivatives is variable for each equation.
.sp
Code pointers are copied into the evaluator common by ft_evinit(). This
procedure also allocates space for the derivative code pointers, before
copying them. Space is freed by ft_evfree(). The fitting function is
evaluated for a single point with ft_func(), derivatives are evaluated
for a single point with ft_dfunc(), and the plot equations are evaluated
for all points with ft_plot(). Plot equations are computed for each
axis separately (two calls).

Entry points:

.nf

    ft_evinit (sym, npars)			        Initialize fit
    ft_evfree ()				        Free space allocated
    ft_func (x, nvars, p, npars, z)		        Fitting function called
    ft_dfunc (x, nvars, p, dp, npars, z, der)	        Derivatives of fit func.
    ft_plot (number, p, npars, x, y, z, npts, nvars)    Plotting functions
.fi
.endhelp

include	<error.h>
include	"../lib/parser.h"

# Pointer Mem
define	MEMP	Memi


# FT_EVINIT - Initialize fit by copying fitting function, its derivative,
# and plotting function codes into the fit common.

procedure ft_evinit (sym, npars)

pointer	sym			# equation symbol
int	npars			# number of parameters

int	np			# parameter counter
#bool	clgetb()
pointer	pr_gsymp(), pr_gderp()

include	"fteval.com"

begin
	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf ("ft_evinit: (sym=%d) (npars=%d)\n")
		#call pargi (sym)
		#call pargi (npars)
	#}

	# Set fitting function code.
	eqcode = pr_gsymp (sym, PTEQRPNFIT)
	if (eqcode == NULL)
	    call error (0, "ft_evinit: Null function equation code")

	# Allocate space for derivative code pointers, and copy them
	# from the symbol table. This is to avoid an unnecessary 
	# overhead during the derivative evaluations.

	call malloc (dercode, npars, TY_POINTER)
	do np = 1, npars {
	    MEMP[dercode + np - 1] = pr_gderp (sym, np, PTEQRPNDER)
	    #if (MEMP[dercode + np - 1] == NULL)
		#call error (0, "ft_evinit: Null derivative equation code")
	}

	# Set plotting equation codes. They could be null.
	xpcode = pr_gsymp (sym, PTEQRPNXPLOT)
	ypcode = pr_gsymp (sym, PTEQRPNYPLOT)

	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf ("ft_evinit: (eqcode=%d) ")
		#call pargi (eqcode)
	    #do np = 1, npars {
	        #call eprintf (" (dercode%d=%d)")
		    #call pargi (np)
		    #call pargi (MEMP[dercode + np - 1])
	    #}
	    #call eprintf (" (xpcode=%d) (ypcode=%d)\n")
		#call pargi (xpcode)
		#call pargi (ypcode)
	#}
end


# FT_EVFREE - Free space used in the fit common.

procedure ft_evfree ()

include	"fteval.com"

#bool	clgetb()

begin
	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf (
	       #"ft_evfree: (eqcode=%d) (dercode=%d) (xpcode=%d) (ypcode=%d)\n")
		#call pargi (eqcode)
		#call pargi (dercode)
		#call pargi (xpcode)
		#call pargi (ypcode)
	#}

	# Clear code pointers.
	eqcode = NULL
	xpcode = NULL
	ypcode = NULL

	# Free derivative buffer.
	call mfree (dercode, TY_POINTER)
end


# FT_FUNC - Evaluate fitting function. This function must conform in
# number and type of arguments to the requirements of the nlfit/inlfit
# packages even though some arguments are not used in this case.

procedure ft_func (x, nvars, p, npars, z)

real	x[ARB]			# independent values
int	nvars			# number of variables
real	p[ARB]			# parameter values
int	npars			# number of parameters
real	z			# function value (output)

include	"fteval.com"

real	pr_eval()

begin
	# Evaluate the function value.
	z = pr_eval (eqcode, x, p)
end


# FT_DFUNC - Evaluate fitting function, and derivatives of the fitting
# function with respect to all the parameters.  This function must conform in
# number and type of arguments to the requirements of the nlfit/inlfit
# packages even though some arguments are not used in this case.

procedure ft_dfunc (x, nvars, p, dp, npars, z, der)

real	x[ARB]			# independent values
int	nvars			# number of variables
real	p[ARB]			# parameter values
real	dp[ARB]			# parameter derivatives
int	npars			# number of parameters
real	z			# function value (output)
real	der[ARB]		# derivative values (output)

int	i
pointer	code
real	pi, zplus, zminus
real	pr_eval()

include	"fteval.com"

begin
	# Evaluate the function value.
	z = pr_eval (eqcode, x, p)

	# Evaluate each one of the derivatives.
	do i = 1, npars {
	    code = MEMP[dercode+i-1]
	    if (code != NULL)
	        der[i] = pr_eval (code, x, p)
	    else {
		pi = p[i]
		p[i] = pi + 0.5 * dp[i] 
		zplus = pr_eval (eqcode, x, p)
		p[i] = pi - 0.5 * dp[i]
		zminus = pr_eval (eqcode, x, p)
		der[i] = (zplus - zminus) / dp[i]
		p[i] = pi
	    }
	}
end


# FT_PLOT - Evaluate plot function(s) for all points.

procedure ft_plot (number, p, npars, x, y, z, npts, nvars)

int	number			# plot number
real	p[npars]		# parameter values
int	npars			# number of parameters (not used)
real	x[ARB]			# independent values
real	y[npts]			# dependent values (not used)
real	z[npts]			# function values (output)
int	npts			# number of points
int	nvars			# number of variables

int	i
pointer	code
real	pr_eval()

include	"fteval.com"

begin
	# Determine plot equation to evaluate.
	if (number == 1)
	    code = xpcode
	else
	    code = ypcode

	# Iterate over input points.
	do i = 1, npts
	    z[i] = pr_eval (code, x[(i-1)*nvars+1], p)
end
