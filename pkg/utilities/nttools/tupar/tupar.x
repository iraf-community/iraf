include <fset.h>		# used to check whether I/O is redirected
include <error.h>
include <tbset.h>
include "tupar.h"		# defines TUPAR_EXIT, TUPAR_QUIT

# tupar -- edit header parameters
# This task may be used to list, add to, replace, or delete header
# parameters in a table or list of tables.
#
# Phil Hodge, 22-Jul-1987  Task created
# Phil Hodge, 11-Aug-1987  Call tbhad[] instead of tbhpt[].
# Phil Hodge, 18-Mar-1988  Rewrite, allowing a list of tables.
# Phil Hodge,  7-Sep-1988  Change parameter name for table.
# Phil Hodge, 23-Aug-1991  Allow quit or exit.
# Phil Hodge,  9-Jul-1993  Allow quit without verification if nothing changed.
# Phil Hodge, 29-Jun-1995  Modify for FITS tables; modify tu_open and tu_close.
# Phil Hodge,  3-Oct-1995  Use tbn instead of fnt.
# Phil Hodge, 22-May-1996  Use iferr when calling tu_instr.

define	LEN_ISBUF	1000	# length or increment for instruction buffer

procedure tupar()

pointer tlist			# for list of input table names
bool	same_for_all		# same set of instructions for all tables?
bool	verbose			# print name of each table?
bool	readonly		# open tables readonly?
#--
pointer tp			# pointer to table descriptor
pointer sp
pointer tname			# scratch for table name
pointer tabname			# scratch for full name of table (incl [...])
pointer lbuf			# scratch for input buffer
pointer isbuf			# buffer for saving instructions
int	bufsize			# allocated size of Memc[isbuf]
int	ibp			# index in Memc[isbuf]
int	eq_flag			# exit or quit
int	istat			# set by tu_ex_instr; > 0 implies error
bool	inplace			# open tables inplace?
bool	modified		# true if the header was modified
bool	from_stdin		# get input from STDIN?
bool	save_instr		# save instruction in buffer?
bool	iredir			# is input redirected?
bool	oredir			# is output redirected?
bool	prompt			# prompt user for input?
bool	alldone			# done with all tables?
bool	done			# done with current table?
bool	quit			# true if we should delete temp table
bool	clgetb()
int	fstati()
pointer tbnopenp()
int	tbnget(), tbnlen()

begin
	call smark (sp)
	call salloc (tname, SZ_LINE, TY_CHAR)
	call salloc (tabname, SZ_LINE, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	tlist = tbnopenp ("table")

	same_for_all = clgetb ("same")
	verbose = clgetb ("verbose")
	readonly = clgetb ("readonly")
	if (readonly)
	    inplace = true
	else
	    inplace = clgetb ("inplace")

	from_stdin = true

	# Is input or output redirected?
	iredir = (fstati (STDIN, F_REDIR) == YES)
	oredir = (fstati (STDOUT, F_REDIR) == YES)
	prompt = !iredir		# prompt if input is not redirected

	save_instr = (same_for_all && (tbnlen (tlist) > 1))
	if (save_instr) {
	    bufsize = LEN_ISBUF
	    call malloc (isbuf, bufsize, TY_CHAR)
	    Memc[isbuf] = EOS
	} else {
	    bufsize = 0
	    isbuf = NULL
	}

	# Loop over all table names in the file name template.
	alldone = (tbnget (tlist, Memc[tname], SZ_LINE) == EOF)
	while (!alldone) {

	    iferr {
		# Open the table (or a copy of it).
		call tu_open (Memc[tname], "tupar", readonly, inplace,
			tp, Memc[tabname], SZ_LINE)
	    } then {
		call eprintf ("can't open %s\n")
		    call pargstr (Memc[tname])
		alldone = (tbnget (tlist, Memc[tname], SZ_LINE) == EOF)
		call erract (EA_WARN)
		next				# ignore this table
	    }

	    if (verbose) {
		if (oredir) {
		    call eprintf ("%s\n")
			call pargstr (Memc[tabname])
		}
		call printf ("%s\n")
		    call pargstr (Memc[tabname])
	    }

	    # Edit header parameters in current table.
	    ibp = 1				# may be incremented in loop
	    modified = false			# may be reset within tu_instr
	    done = false
	    while ( ! done ) {
		# Get an instruction, execute it, and possibly save it
		# for use again.
		iferr {
		    call tu_instr (tp, Memc[lbuf], readonly, prompt, from_stdin,
			iredir, save_instr, isbuf, bufsize, ibp,
			modified, eq_flag, done, istat)
		} then {
		    call erract (EA_WARN)
		}
		if (istat > 0)
		    call eprintf ("table was opened readonly\n")

		if (inplace && !readonly &&
			(eq_flag == TUPAR_QUIT || eq_flag == TUPAR_QUIT_NC)) {
		    call eprintf (
"can't quit without saving changes because you edited the table inplace\n")
		    done = false
		} else if (eq_flag == TUPAR_QUIT && modified && !readonly) {
		    # Ask for verification before quitting.
		    call clputb ("go_ahead", clgetb ("quit_default"))
		    call eprintf ("quit without saving changes")
		    call flush (STDERR)
		    if (! clgetb ("go_ahead"))
			done = false		# no, don't quit
		}
	    }

	    # Reset flags after processing first table.
	    if (same_for_all) {
		prompt = false
		from_stdin = false
	    }
	    save_instr = false

	    # Close the table, renaming the temp table back to the
	    # original if we are saving our changes.
	    quit = (eq_flag == TUPAR_QUIT || eq_flag == TUPAR_QUIT_NC)
	    iferr {
		call tu_close (tp, inplace, quit, Memc[tabname])
	    } then {
		alldone = (tbnget (tlist, Memc[tname], SZ_LINE) == EOF)
		call erract (EA_WARN)
		next				# ignore this table
	    }

	    # Get the name of the next file in the list.
	    alldone = (tbnget (tlist, Memc[tname], SZ_LINE) == EOF)

	    # If the user modified the table but then decided to quit,
	    # then abort without opening the rest of the tables.
	    if (eq_flag != TUPAR_EXIT && modified && same_for_all)
		alldone = true
	}
	if (isbuf != NULL)
	    call mfree (isbuf, TY_CHAR)
	call tbnclose (tlist)
	call sfree (sp)
end


# tu_save_instr -- save edit instruction
# Save the current instruction in the instruction buffer.  The entries
# are separated by '\n', and the entire set of entries is terminated
# by EOS.
# If the buffer would overflow, it will be reallocated.  

procedure tu_save_instr (lbuf, isbuf, bufsize, ibp)

char	lbuf[ARB]		# i: line buffer containing instruction
pointer isbuf			# io: buffer for saving instructions
int	bufsize			# io: current allocated size of Memc[isbuf]
int	ibp			# io: current index in Memc[isbuf]
#--
int	k			# loop index
bool	done			# loop-termination flag
int	leni			# length of lbuf
int	strlen()

begin
	leni = strlen (lbuf)
	if (ibp + leni >= bufsize) {
	    bufsize = bufsize + LEN_ISBUF
	    call realloc (isbuf, bufsize, TY_CHAR)
	}

	done = false
	k = 1
	while ( ! done ) {
	    if ((lbuf[k] == EOS) || (lbuf[k] == '\n')) {
		done = true
	    } else {
		Memc[isbuf+ibp-1] = lbuf[k]
		ibp = ibp + 1
	    }
	    k = k + 1
	}
	Memc[isbuf+ibp-1] = '\n'
	Memc[isbuf+ibp] = EOS
	ibp = ibp + 1			# so ibp points to EOS
end


# tu_rd_instr -- read edit instruction
# Read an instruction from the instruction buffer.  When EOS is reached
# in the buffer, an EOF will be returned; otherwise, the number of char
# in the current instruction will be returned.

int procedure tu_rd_instr (isbuf, ibp, lbuf)

char	isbuf[ARB]		# i: buffer containing instructions
int	ibp			# io: current index in isbuf
char	lbuf[ARB]		# o: buffer to receive instruction
#--
int	k			# loop index
bool	done			# loop-termination flag

begin
	done = false
	k = 0
	while ( ! done ) {

	    if (isbuf[ibp] == '\n') {

		if (k > 0)		# skip past adjacent '\n'
		    done = true
		ibp = ibp + 1

	    } else if (isbuf[ibp] == EOS) {

		done = true		# leave ibp pointing to EOS

	    } else {

		k = k + 1
		lbuf[k] = isbuf[ibp]
		ibp = ibp + 1
	    }
	}
	lbuf[k+1] = EOS

	if (k <= 0)
	    k = EOF
	return (k)
end
