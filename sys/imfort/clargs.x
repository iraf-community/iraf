# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imfort.h"

.help clargs
.nf --------------------------------------------------------------------------
CLARGS.X -- Command Line Argument access package for IMFORT.

The CLARGS package provides access to the foreign task command line, if any,
passed to the IMFORT program when it was run.  The raw command line may be
obtained as a string, the individual arguments may be extracted as strings,
or arguments may be decoded as integer or floating point values.

	clnarg (nargs)			# get number of command line arguments
	clargc (argno, sval, ier)	# get argument argno as a string
	clargi (argno, ival, ier)	# get argument argno as an integer
	clargr (argno, rval, ier)	# get argument argno as a real
	clargd (argno, dval, ier)	# get argument argno as a double
	clrawc (cmdstr, ier)		# get entire raw command line

Command line arguments are delimited by whitespace.  String arguments do not
have to be quoted; string arguments containing whitespace must be quoted.
FMTIO is used to decode numeric arguments, hence the IRAF notations are
recognized for radix specification (octal, hex) and for sexagesimal input.

Note that a Fortran program using IMFORT may be interfaced to the IRAF CL
as a foreign task, using the CLARGS interface to pass foreign task command
line arguments to the Fortran program, allowing user written Fortran programs
to be called from within CL scripts as well as interactively.
.endhelp ---------------------------------------------------------------------


# CLARGC -- Return the indicated whitespace delimited command line argument
# as a string.

procedure clargc (argno, outstr, ier)

int	argno				# desired argument
%	character*(*) outstr
int	ier

int	u_nargs
int	u_argp[MAX_ARGS]
char	u_sbuf[SZ_CMDLINE]
common	/argcom/ u_nargs, u_argp, u_sbuf

begin
	call cl_initargs (ier)
	if (ier > 0)
	    return

	if (argno < 1 || argno > u_nargs)
	    ier = IE_NEXARG
	else {
	    call f77pak (u_sbuf[u_argp[argno]], outstr, len(outstr))
	    ier = OK
	}
end


# CLARGI -- Return the indicated whitespace delimited command line argument
# as an integer.

procedure clargi (argno, ival, ier)

int	argno			# desired argument
int	ival			# integer value of argument
int	ier

double	dval

begin
	call clargd (argno, dval, ier)
	if (ier == OK)
	    ival = dval		# (integer overflow if large exponent)
end


# CLARGR -- Return the indicated whitespace delimited command line argument
# as a real.

procedure clargr (argno, rval, ier)

int	argno			# desired argument
real	rval			# integer value of argument
int	ier

double	dval

begin
	call clargd (argno, dval, ier)
	if (ier == OK)
	    rval = dval
end


# CLARGD -- Return the indicated whitespace delimited command line argument
# as a double.

procedure clargd (argno, dval, ier)

int	argno			# desired argument
double	dval			# double floating value of argument
int	ier

int	ip, gctod()

int	u_nargs
int	u_argp[MAX_ARGS]
char	u_sbuf[SZ_CMDLINE]
common	/argcom/ u_nargs, u_argp, u_sbuf

begin
	call cl_initargs (ier)
	if (ier > 0)
	    return

	if (argno < 1 || argno > u_nargs)
	    ier = IE_NEXARG
	else {
	    ip = u_argp[argno]
	    if (gctod (u_sbuf, ip, dval) <= 0) {
		ier = IE_NONNUMARG
		call im_seterrop (ier, u_sbuf[ip])
	    } else
		ier = OK
	}
end


# CLNARG -- Return the number of command line arguments.

procedure clnarg (nargs)

int	nargs
int	ier

int	u_nargs
int	u_argp[MAX_ARGS]
char	u_sbuf[SZ_CMDLINE]
common	/argcom/ u_nargs, u_argp, u_sbuf

begin
	call cl_initargs (ier)
	if (ier != OK)
	    nargs = 0
	else
	    nargs = u_nargs
end


# CL_INITARGS -- The first time we are called, read the raw command line
# and parse it into the individual argument strings in the ARGCOM common.
# After the first call the common is set and we are a no-op.

procedure cl_initargs (ier)

int	ier

int	status, op
bool	first_time
pointer	sp, cmd, token, ip
data	first_time /true/
int	ctowrd(), gstrcpy()

int	u_nargs
int	u_argp[MAX_ARGS]
char	u_sbuf[SZ_CMDLINE]
common	/argcom/ u_nargs, u_argp, u_sbuf

begin
	if (!first_time) {
	    ier = OK
	    return
	}

	call smark (sp)
	call salloc (cmd, SZ_CMDLINE, TY_CHAR)
	call salloc (token, SZ_CMDLINE, TY_CHAR)

	call zgcmdl (Memc[cmd], SZ_CMDLINE, status)
	if (status <= 0) {
	    ier = IE_GCMDLN
	    call sfree (sp)
	    return
	}
	
	call strupk (Memc[cmd], Memc[cmd], SZ_CMDLINE)
	u_nargs = 0
	ip = cmd
	op = 1

	while (ctowrd (Memc, ip, Memc[token], SZ_CMDLINE) > 0) {
	    u_nargs = u_nargs + 1
	    u_argp[u_nargs] = op
	    op = op + gstrcpy (Memc[token], u_sbuf[op], SZ_CMDLINE-op+1) + 1
	}

	ier = OK
	first_time = false
	call sfree (sp)
end


# CLRAWC -- Get the raw command line passed by the host system when the calling
# program was run.  This should be the command line entered in the CL when the
# program was called, assuming that the program is implemented as a foreign task
# in the CL.

procedure clrawc (outstr, ier)

%	character*(*) outstr
int	ier

int	status
pointer	sp, cmd

begin
	call smark (sp)
	call salloc (cmd, SZ_CMDLINE, TY_CHAR)

	call zgcmdl (Memc[cmd], SZ_CMDLINE, status)
	if (status <= 0)
	    ier = IE_GCMDLN
	else {
	    call strupk (Memc[cmd], Memc[cmd], SZ_CMDLINE)
	    call f77pak (Memc[cmd], outstr, len(outstr))
	    ier = OK
	}

	call sfree (sp)
end
