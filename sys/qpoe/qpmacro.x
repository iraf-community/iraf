# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <error.h>
include <finfo.h>
include <ctype.h>
include "qpoe.h"
include "qpex.h"

.help qpmacro
.nf ---------------------------------------------------------------------------
QPMACRO -- Macro facility for QPOE.  QPOE permits macro replacement in various
places, e.g., to alias parameter names, or enter predefined selection
expressions (selection functions).  While macros may be defined permanently
in the datafile, they are more commonly defined by the user at runtime, and
used as a global facility to access any number of datafiles.  Since we do not
want to store runtime macros in the datafile headers, the compiled definitions
cannot be entered into the datafile symbol table, but must be entered into a
separate global symbol table, maintained by QPOE and used to store runtime
macros to be used in all datafile accesses.

The purpose of the package is to maintain an up to date global macro symbol
table.	The symbol table itself is directly accessed by the client program,
rather than via the package interface, so that the standard SYMTAB package
routines may be used to access the symbol table.

	 qm = qm_access ()
	 st = qm_symtab (qm)
	 qm_setdefaults (qm, qp)
	 qm_upddefaults (qm, qp)

		qm_scan (qm, fname, flags)
	       qm_scano (qm, fd, flags)

The macro symbol table is accessed with QM_ACCESS, which will compile or
update the in-core version of the symbol table if necessary.  A call to
QM_SYMTAB is required to obtain the symbol table descriptor, a pointer,
which may change any time the symbol table is modified.  QM_ACCESS should
be called only occasionally (e.g., at datafile open time) since it makes a
number of system calls to check file dates.  QM_SYMTAB should be called
once upon entry to every routine which accesses the macro database.

QM_SETDEFAULTS is called when a datafile is opened to set the default values
of all interface and datafile parameters; the user can control these defaults
by including SET statements in the macro definition file.  QM_UPDDEFAULTS is
similar, except that it sets ONLY the values of those parameters that have
been explicitly set in SET statements in the macro files.

When QM_ACCESS is called it looks for two variables in the user environment.

	QMSAVE		The name of the file (default home$qpoe.msv) in
			which the compiled macro database is to be saved,
			or from which it is to be loaded.

	QMFILES		A comma delimited list of macro definition (MD)
			files to be scanned to compile the macro database.
			(No default unless one is supplied by the local
			system administrator).

If the binary symbol table file QMSAVE exists and is newer than any of the MD
files then the symbol table is reloaded from the binary save file, else the MD
files are scanned and we attempt to write a new save file when done.  If the
symbol table is already open and is newer than either the save file or the
MD files, then the routine exits immediately, returning a pointer to the global
QPOE macro database (symbol table).  QM_SCAN and QM_SCANO are low level
routines for reading the contents of a MD file or stream into the symbol table.

Note that at runtime, a completely different facility exists for macro
replacement; macros may be drawn from either source.  The second mechanism
takes the name of the macro to be the *name* of a file in the current
directory containing the value string for the macro.  This is less efficient,
but allows macros to be independently created and used dynamically at runtime.
The latter type of macros may be referenced only in QPOE selection expressions
(rather than as, for example, parameter name aliases).
.endhelp ----------------------------------------------------------------------

# Size limiting definitions.
define	QM_MAXFILES	32		# maximum files in QMFILES list
define	QM_SZCBUF	1024		# char storage for file list
define	DEF_LENINDEX	50		# symbol table (init hash index)
define	DEF_LENSTAB	256		# symbol table (init table len)
define	DEF_SZSBUF	1024		# symbol table (init string buf len)
define	SZ_MNAME	32		# max size macro name
define	SZ_MVBUF	8192		# max size macro value

# Defined parameters.
define	QMFILES		"qmfiles"	# macro define file list
define	QMSAVE		"qmsave"	# symtab save file
define	DEF_QMSAVE	"uparm$qpoe.msv" # default macro save file
define	STTIME		"$STTIME"	# time of last st compile
define	PSETKW		"$PSETKW"	# param used to store SET values
define	QMSTNAME	"QPOEMACROS"	# symbol table name

# Flags for QM_SCAN.
define	QM_FLAGREDEFS	1B		# complain about redefined macros

# The QM descriptor (fixed pointer, while QM_ST is allowed to change).
define	LEN_QM		1
define	QM_ST		Memi[$1]	# pointer to macro symbol table

# The parameter set descriptor (for SET statements).
define	LEN_PSET	32		# allow some extra space
define	PS_EXPBLEN	Memi[$1]	# QPEX program buffer length
define	PS_EXDBLEN	Memi[$1+1]	# QPEX data buffer length
define	PS_EXMAXFRLLEN	Memi[$1+2]	# QPEX max FRLUT length
define	PS_EXMAXRRLLEN	Memi[$1+3]	# QPEX max RRLUT length
define	PS_EXLMINRANGES	Memi[$1+4]	# QPEX max ranges before using LUT
define	PS_EXLSCALE	Memi[$1+5]	# QPEX scale nranges to LUT bins
define	PS_SZPBBUF	Memi[$1+6]	# size of pushback buffer for macros
define	PS_BUCKETLEN	Memi[$1+7]	# QPIO event file bucket size
define	PS_FMMAXLFILES	Memi[$1+8]	# FMIO maxlfiles
define	PS_FMPAGESIZE	Memi[$1+9]	# FMIO pagesize
define	PS_FMCACHESIZE	Memi[$1+10]	# FMIO buffer cache size
define	PS_STINDEXLEN	Memi[$1+11]	# SYMTAB hash index length
define	PS_STSTABLEN	Memi[$1+12]	# SYMTAB stab len (start)
define	PS_STSBUFSIZE	Memi[$1+13]	# SYMTAB sbuf size (start)
define	PS_BLOCK	Memi[$1+14]	# QPIO blocking factor
define	PS_DEBUG	Memi[$1+15]	# debug level
define	PS_OPTBUFSIZE	Memi[$1+16]	# QPIO/QPF FIO optimum buffer size 

# Handy macros.
define	IS_PUNCT	(IS_WHITE($1)||($1)==','||($1)=='\n')


# QM_ACCESS -- Access the QPOE macro descriptor.  Once opened, this should
# remain open for the lifetime of the process.	Since these macros are global,
# the single descriptor is shared by all open datafiles and all tasks in the
# process.

pointer procedure qm_access()

int	nfiles, fd, i
bool	save_file_exists
pointer	file[QM_MAXFILES]
long	fi[LEN_FINFO], date[QM_MAXFILES], stdate
pointer sp, qmfiles, qmsave, cbuf, qm, st, st_start, start, sym, ps, ip, op

long	clktime()
int	envfind(), finfo(), open()
pointer stopen(), stenter(), stfind(), strestore()
errchk	stopen, malloc, syserrs
string	sttimekw STTIME
define	uptodate_ 91
data	qm /NULL/

begin
	call smark (sp)
	call salloc (qmfiles, SZ_LINE, TY_CHAR)
	call salloc (qmsave, SZ_PATHNAME, TY_CHAR)
	call salloc (cbuf, QM_SZCBUF, TY_CHAR)

	# Open the QM descriptor only once (per process).
	if (qm == NULL) {
	    # Allocate descriptor.
	    call malloc (qm, LEN_QM, TY_STRUCT)

	    # Initialize symbol table.
	    st = stopen (QMSTNAME, DEF_LENINDEX, DEF_LENSTAB, DEF_SZSBUF)
	    sym = stenter (st, sttimekw, SZ_LONG);  Meml[sym] = 0

	    # Initialize settable interface/datafile parameters.
	    ps = stenter (st, PSETKW, LEN_PSET)
	    call aclri (Memi[ps], LEN_PSET)

	    # Free back to here when rebuilding symbol table.
	    call stmark (st, st_start)
	}

	# Get the QMSAVE symtab save file filename.
	if (envfind (QMSAVE, Memc[qmsave], SZ_PATHNAME) <= 0)
	    call strcpy (DEF_QMSAVE, Memc[qmsave], SZ_PATHNAME)

	# Get the QMFILES macro define file list.
	if (envfind (QMFILES, Memc[qmfiles], SZ_LINE) <= 0)
	    Memc[qmfiles] = EOS

	# Process the QMFILES string into a list of filenames, and get the
	# modify date of each file.

	nfiles = 0
	op = cbuf
	ip = qmfiles

	repeat {
	    # Get the next comma delimited argument from QMFILES.
	    while (IS_PUNCT(Memc[ip]))
		ip = ip + 1

	    start = op
	    while (Memc[ip] != EOS && !IS_PUNCT(Memc[ip])) {
		Memc[op] = Memc[ip]
		op = op + 1
		ip = ip + 1
	    }

	    Memc[op] = EOS
	    op = op + 1
	    if (Memc[start] == EOS)
		break

	    # Add the file and its modify date to the file list.
	    if (finfo (Memc[start], fi) == ERR) {
		call eprintf ("Warning: QPOE macro file %s not found\n")
		    call pargstr (Memc[start])
	    } else {
		nfiles = nfiles + 1
		if (nfiles > QM_MAXFILES)
		    call syserrs (SYS_QMNFILES, Memc[qmsave])
		file[nfiles] = start
		date[nfiles] = fi[FI_MTIME]
	    }
	}

	# Check the dates of the MD files against the in-core symbol table
	# and exit if the symbol table is up to date.	The date of the symbol
	# table is stored in the table itself.

	sym = stfind (st, sttimekw)
	if (nfiles > 0 && sym != NULL) {
	    stdate = Meml[sym]
	    for (i=1;  i <= nfiles;  i=i+1)
		if (date[i] > stdate)
		    break
	    if (i > nfiles)
		goto uptodate_
	}

	# If a binary symtab save file exists and is up to date, load it
	# into the descriptor.

	save_file_exists = false
	if (nfiles > 0)
	    save_file_exists = (finfo (Memc[qmsave], fi) == OK)

	if (save_file_exists) {
	    stdate = fi[FI_MTIME]
	    for (i=1;  i <= nfiles;  i=i+1)
		if (date[i] > stdate)
		    break
	    if (i > nfiles || nfiles == 0) {
		iferr (fd = open (Memc[qmsave], READ_ONLY, BINARY_FILE)) {
		    call eprintf ("Warning: cannot open ")
		    call eprintf ("QPOE macro save file %s\n")
			call pargstr (Memc[qmsave])
		} else {
		    call stclose (st)
		    st = strestore (fd)
		    call close (fd)
		    goto uptodate_
		}
	    }
	}

	# If we get here then we need to scan the MD files and build a new
	# symbol table.

	# Rebuild the symbol table.
	call stfree (st, st_start)
	call stmark (st, st_start)
	QM_ST(qm) = st

	for (i=1;  i <= nfiles;  i=i+1)
	    iferr (call qm_scan (qm, Memc[file[i]], 0))
		call erract (EA_WARN)

	# Set the time of last update.
	Meml[sym] = clktime (0)

	# Update the save file if we have any defined macros.
	if (nfiles > 0) {
	    call intr_disable()
	    if (save_file_exists)
		iferr (call delete (Memc[qmsave]))
		    call erract (EA_WARN)
	    iferr (fd = open (Memc[qmsave], NEW_FILE, BINARY_FILE))
		call erract (EA_WARN)
	    else {
		iferr (call stsave (st, fd))
		    call erract (EA_WARN)
		call close (fd)
	    }
	    call intr_enable()
	}

uptodate_
	call sfree (sp)
	QM_ST(qm) = st
	return (qm)
end


# QM_SYMTAB -- Get a pointer to the symbol table used to store the defined
# macros for QPOE.  The level of indirection is needed so that the QM pointer
# can be fixed while the symtab pointer is allowed to change as the symbol
# table is modified or rebuilt.

pointer procedure qm_symtab (qm)

pointer qm			#I QM descriptor

begin
	return (QM_ST(qm))
end


# QM_SCAN -- Scan a macro definition (MD) file and add any macros defined
# therein into the symbol table.

procedure qm_scan (qm, fname, flags)

pointer qm			#I QM descriptor
char	fname[ARB]		#I MD file name
int	flags			#I scan flags

int	fd
int	open()
errchk	open

begin
	fd = open (fname, READ_ONLY, TEXT_FILE)
	call qm_scano (qm, fd, flags)
	call close (fd)
end


# QM_SCANO -- Scan a stream and parse any macro defines therein, adding the
# defined macros to the given symbol table, and setting the values of the
# specified interface or datafile parameters.
#
# The syntax of a SET statement, used to set the default values of interface
# and datafile parameters, is as follows:
#
#	set parameter value
#
# where the parameter names are as given in <qpset.h> (case not significant).
# Parameter values set in this way may be overridden by QP_SETI calls after
# opening a datafile.
#
# The syntax of a macro define is as follows:
#
#	define name value
#
# where NAME is a simple alphanumeric identifier, and the string VALUE may
# contain references of the form $N, N=0:9, $0 being the macro name, $1:9
# being replaced by the arguments to the macro when it is called.  Newline
# may be escaped to enter multiline macro definition statements.  Comments
# and blank lines are ignored.	During macro expansion, any parenthesized
# arguments following the macro name will be consumed only if the macro as
# defined has symbolic arguments.  The value string will be inserted without
# adding any whitespace at either end, and whitespace within the value string
# is significant.

procedure qm_scano (qm, fd, flags)

pointer qm			#I QM descriptor
int	fd			#I input stream
int	flags			#I scan flags

int	ch
int	symarg, junk, i
bool	is_define, is_set
pointer	sp, mname, mvbuf, sym, st, op, otop

bool	streq()
int	qm_getc(), stpstr()
pointer	stfind(), stenter()
errchk	qm_getc, qm_setparam, stenter, stpstr
define	next_ 91

begin
	call smark (sp)
	call salloc (mname, SZ_MNAME, TY_CHAR)
	call salloc (mvbuf, SZ_MVBUF, TY_CHAR)

	st = QM_ST(qm)
	junk = qm_getc (NULL, ch)

	# Each loop processes one newline delimited statement from the
	# input stream.  The qm_getc function deals with continuation,
	# blank lines and comments, etc.

	repeat {
	    # Get `define' and macro name (or `set' and parameter name).
next_
	    do i = 1, 2 {
		# Get identifier token.
		op = mname
		otop = mname + SZ_MNAME - 1
		while (qm_getc (fd, ch) != EOF) {
		    if (IS_ALNUM(ch) || ch == '_') {
			Memc[op] = ch
			op = min (otop, op + 1)
		    } else if (ch == '\n') {
			goto next_
		    } else if (IS_WHITE(ch) && op == mname) {
			next
		    } else
			break
		}
		Memc[op] = EOS

		# Process statement type keyword.
		if (i == 1) {
		    is_define = (streq (Memc[mname], "define"))
		    is_set    = (streq (Memc[mname], "set"))

		    # Ignore statements other than SET or DEFINE.
		    if (!(is_define || is_set)) {
			while (qm_getc (fd, ch) != EOF)
			    if (ch == '\n')
				goto next_
		    }
		}
	    }

	    # Check for EOF.
	    if (Memc[mname] == EOS)
		break

	    # Skip optional "=" if SET statement.
	    if (is_set)
		while (IS_WHITE(ch)) {
		    if (qm_getc (fd, ch) == EOF)
			break
		    else if (ch == '\n')
			break
		    else if (ch == '=')
			ch = ' '
		}

	    # Skip to value string; leave first char in ch.
	    while (IS_WHITE(ch)) {
		if (qm_getc (fd, ch) == EOF)
		    break
		else if (ch == '\n')
		    break
	    }

	    # Get value string.  Check for the presence of any symbolic
	    # arguments of the form $N in the process.

	    otop = mvbuf + SZ_MVBUF - 1
	    op = mvbuf
	    symarg = 0

	    Memc[op] = ch
	    op = op + 1

	    while (qm_getc (fd, ch) != EOF)
		if (ch == '\n')
		    break
		else {
		    Memc[op] = ch
		    if (IS_DIGIT(ch))
			if (op > mvbuf)
			    if (Memc[op-1] == '$')
				symarg = max (symarg, TO_INTEG(ch))
		    op = min (otop, op + 1)
		}
	    Memc[op] = EOS

	    # Process SET statements.
	    if (is_set) {
		call strlwr (Memc[mname])
		call qm_setparam (qm, Memc[mname], Memc[mvbuf])
		next
	    }

	    # Check for a redef.
	    if (and (flags, QM_FLAGREDEFS) != 0) {
		sym = stfind (st, Memc[mname])
		if (sym != NULL) {
		    call eprintf ("Warning: QPOE macro `%s' redefined\n")
			call pargstr (Memc[mname])
		}
	    }

	    # Enter the macro into the symbol table.
	    sym = stenter (st, Memc[mname], LEN_SYMBOL)
	    S_OFFSET(sym) = stpstr (st, Memc[mvbuf], 0)
	    S_DTYPE(sym) = TY_MACRO
	    S_FLAGS(sym) = 0
	    if (symarg > 0)
		S_FLAGS(sym) = SF_MACARGS
	    else
		S_FLAGS(sym) = 0
	}

	call sfree (sp)
end


# QM_SETPARAM -- Set the default value of an interface or datafile parameter.

procedure qm_setparam (qm, param, valstr)

pointer	qm			#I QM descriptor
char	param[ARB]		#I parameter to be set
char	valstr[ARB]		#I parameter value

pointer	ps
int	value, ip
int	qp_ctoi(), strncmp()
pointer	stfind()
bool	streq()
errchk	stfind

begin
	ps = stfind (QM_ST(qm), PSETKW)
	if (ps == NULL)
	    return

	# Decode the parameter value - only integer parameters at present.
	ip = 1
	if (qp_ctoi (valstr, ip, value) <= 0) {
	    call eprintf ("bad value `%s' for QPOE parameter `%s'\n")
		call pargstr (valstr)
		call pargstr (param)
	    return
	}

	# Accept either QP_PARAM or just PARAM.
	ip = 1
	if (strncmp (param, "qp_", 3) == 0)
	    ip = 4

	# Set the parameter value in the global QM descriptor.
	if (     streq (param[ip], "bucketlen"))
	    PS_BUCKETLEN(ps) = value
	else if (streq (param[ip], "cachesize"))
	    PS_FMCACHESIZE(ps) = value
	else if (streq (param[ip], "indexlen"))
	    PS_STINDEXLEN(ps) = value
	else if (streq (param[ip], "maxlfiles"))
	    PS_FMMAXLFILES(ps) = value
	else if (streq (param[ip], "pagesize"))
	    PS_FMPAGESIZE(ps) = value
	else if (streq (param[ip], "sbufsize"))
	    PS_STSBUFSIZE(ps) = value
	else if (streq (param[ip], "stablen"))
	    PS_STSTABLEN(ps) = value
	else if (streq (param[ip], "progbuflen"))
	    PS_EXPBLEN(ps) = value
	else if (streq (param[ip], "databuflen"))
	    PS_EXDBLEN(ps) = value
	else if (streq (param[ip], "maxfrlutlen"))
	    PS_EXMAXFRLLEN(ps) = value
	else if (streq (param[ip], "maxrrlutlen"))
	    PS_EXMAXRRLLEN(ps) = value
	else if (streq (param[ip], "lutminranges"))
	    PS_EXLMINRANGES(ps) = value
	else if (streq (param[ip], "lutscale"))
	    PS_EXLSCALE(ps) = value
	else if (streq (param[ip], "maxpushback"))
	    PS_SZPBBUF(ps) = value
	else if (streq (param[ip], "blockfactor"))
	    PS_BLOCK(ps) = value
	else if (streq (param[ip], "debuglevel"))
	    PS_DEBUG(ps) = value
	else if (streq (param[ip], "optbufsize"))
	    PS_OPTBUFSIZE(ps) = value
	else {
	    call eprintf ("unknown QPOE parameter `%s' in SET statement\n")
		call pargstr (param)
	}
end


# QM_SETDEFAULTS -- Set the current default values of all interface and
# datafile parameters in a QPOE descriptor.  Called at datafile open time
# to get the defaults.

procedure qm_setdefaults (qm, qp)

pointer	qm			#I QM descriptor
pointer	qp			#I QPOE descriptor

pointer	ps
pointer	stfind()
int	qm_setpar()
errchk	stfind

begin
	ps = stfind (QM_ST(qm), PSETKW)
	if (ps == NULL)
	    return

	# Interface parameters.
	QP_EXPBLEN(qp) = qm_setpar (PS_EXPBLEN(ps), DEF_PROGBUFLEN)
	QP_EXDBLEN(qp) = qm_setpar (PS_EXDBLEN(ps), DEF_DATABUFLEN)
	QP_EXMAXFRLLEN(qp) = qm_setpar (PS_EXMAXFRLLEN(ps), DEF_MAXFRLUTLEN)
	QP_EXMAXRRLLEN(qp) = qm_setpar (PS_EXMAXRRLLEN(ps), DEF_MAXRRLUTLEN)
	QP_EXLMINRANGES(qp) = qm_setpar (PS_EXLMINRANGES(ps), DEF_LUTMINRANGES)
	QP_EXLSCALE(qp) = qm_setpar (PS_EXLSCALE(ps), DEF_LUTSCALE)
	QP_SZPBBUF(qp) = qm_setpar (PS_SZPBBUF(ps), DEF_MAXPUSHBACK)
	QP_FMCACHESIZE(qp) = qm_setpar (PS_FMCACHESIZE(ps), DEF_FMCACHESIZE)

	# Datafile parameters.
	QP_BUCKETLEN(qp) = qm_setpar (PS_BUCKETLEN(ps), DEF_BUCKETLEN)
	QP_FMMAXLFILES(qp) = qm_setpar (PS_FMMAXLFILES(ps), DEF_FMMAXLFILES)
	QP_FMPAGESIZE(qp) = qm_setpar (PS_FMPAGESIZE(ps), DEF_FMPAGESIZE)
	QP_STINDEXLEN(qp) = qm_setpar (PS_STINDEXLEN(ps), DEF_STINDEXLEN)
	QP_STSTABLEN(qp) = qm_setpar (PS_STSTABLEN(ps), DEF_STSTABLEN)
	QP_STSBUFSIZE(qp) = qm_setpar (PS_STSBUFSIZE(ps), DEF_STSBUFSIZE)

	# Other parameters.
	QP_BLOCK(qp) = qm_setpar (PS_BLOCK(ps), DEF_BLOCKFACTOR)
	QP_OPTBUFSIZE(qp) = qm_setpar (PS_OPTBUFSIZE(ps), DEF_OPTBUFSIZE)
	QP_DEBUG(qp) = qm_setpar (PS_DEBUG(ps), 0)
end


# QM_SETPAR -- Return the given parameter value, if set in the user's macro
# files, otherwise return the interface default.

int procedure qm_setpar (userval, defval)

int	userval			#I user specified value, or zero
int	defval			#I interface default

begin
	if (userval != 0)
	    return (userval)
	else
	    return (defval)
end


# QM_UPDDEFAULTS -- Update the values in the QPOE descriptor of all interface
# and datafile parameters set explicitly by a user macro or SET statement.
# Only those parameters for which values were explicitly specified in the
# use macro files are affected, allowing the use of global macros or set
# statements to override the interface or datafile defaults.

procedure qm_upddefaults (qm, qp)

pointer	qm			#I QM descriptor
pointer	qp			#I QPOE descriptor

pointer	ps
pointer	stfind()
errchk	stfind

begin
	ps = stfind (QM_ST(qm), PSETKW)
	if (ps == NULL)
	    return

	# Interface parameters.
	if (PS_EXPBLEN(ps) != 0)	QP_EXPBLEN(qp) = PS_EXPBLEN(ps)
	if (PS_EXDBLEN(ps) != 0)	QP_EXDBLEN(qp) = PS_EXDBLEN(ps)
	if (PS_EXMAXFRLLEN(ps) != 0)	QP_EXMAXFRLLEN(qp) = PS_EXMAXFRLLEN(ps)
	if (PS_EXMAXRRLLEN(ps) != 0)	QP_EXMAXRRLLEN(qp) = PS_EXMAXRRLLEN(ps)
	if (PS_EXLMINRANGES(ps) != 0)	QP_EXLMINRANGES(qp)= PS_EXLMINRANGES(ps)
	if (PS_EXLSCALE(ps) != 0)	QP_EXLSCALE(qp) = PS_EXLSCALE(ps)
	if (PS_SZPBBUF(ps) != 0)	QP_SZPBBUF(qp) = PS_SZPBBUF(ps)
	if (PS_FMCACHESIZE(ps) != 0)	QP_FMCACHESIZE(qp) = PS_FMCACHESIZE(ps)

	# Datafile parameters.
	if (PS_BUCKETLEN(ps) != 0)	QP_BUCKETLEN(qp) = PS_BUCKETLEN(ps)
	if (PS_FMMAXLFILES(ps) != 0)	QP_FMMAXLFILES(qp) = PS_FMMAXLFILES(ps)
	if (PS_FMPAGESIZE(ps) != 0)	QP_FMPAGESIZE(qp) = PS_FMPAGESIZE(ps)
	if (PS_STINDEXLEN(ps) != 0)	QP_STINDEXLEN(qp) = PS_STINDEXLEN(ps)
	if (PS_STSTABLEN(ps) != 0)	QP_STSTABLEN(qp) = PS_STSTABLEN(ps)
	if (PS_STSBUFSIZE(ps) != 0)	QP_STSBUFSIZE(qp) = PS_STSBUFSIZE(ps)

	# Other parameters.
	if (PS_BLOCK(ps) != 0)		QP_BLOCK(qp) = PS_BLOCK(ps)
	if (PS_OPTBUFSIZE(ps) != 0)	QP_OPTBUFSIZE(qp) = PS_OPTBUFSIZE(ps)
	if (PS_DEBUG(ps) != 0)		QP_DEBUG(qp) = PS_DEBUG(ps)
end


# QM_GETC -- Return the next character from the input stream, ignoring
# comments and joining continued lines.  The character value or EOF is
# returned as the function value.  A call with FD=0 will initialize i/o
# for a new file.

int procedure qm_getc (fd, ch)

int	fd			#I input file
int	ch			#O returned character

int	quote
int	getci()
errchk	getci
define	again_ 91

begin
	# Initialization.
	if (fd <= 0) {
	    quote = 0
	    return (0)
	}

	# Handle the most common cases first.
again_
	if (getci (fd, ch) == EOF) {
	    quote = 0
	    return (EOF)
	} else if (IS_ALNUM(ch))
	    return (ch)

	# Handle the special cases - comments, escapes, quoted strings.
	if (ch == '#' && quote == 0) {
	    # Skip a comment.
	    while (getci (fd, ch) != EOF)
		if (ch == '\n')
		    goto again_
	} else if (ch == '\'' || ch == '"') {
	    # Toggle quoted string flag.
	    if (quote == 0)
		quote = ch
	    else if (quote == ch)
		quote = 0
	} else if (ch == '\\') {
	    # Process escapes.
	    if (getci (fd, ch) == '\n')
		goto again_
	    else if (quote == 0 && (ch == '\'' || ch == '"' || ch == '#'))
		;
	    else {
		call ungetci (fd, ch)
		ch = '\\'
	    }
	}

	# Init context at end of every logical line.
	if (ch == '\n')
	    quote = 0

	return (ch)
end
