# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<finfo.h>
include	<fset.h>
include	<error.h>
include	"help.h"

.help help
.nf __________________________________________________________________________
HELP -- Print help documentation for the named modules (tasks, packages,
or library procedures).  Given the package name and module name, the help
database is used to locate the file(s) containing the desired help text.
Documentation is organized by package, and is maintained in the package
directory.  The Lroff text formatter is used to process help text containing
formatting directives.  Text is output via the TTY interface, which processes
standout mode control sequences, and clears the screen between pages.
If the standard output is redirected, output is via putline, and pagination
is disabled.  See Help.hlp for detailed documentation.
.endhelp _____________________________________________________________________

define	SZ_TEMPBUF	SZ_HELPDB


# HELP -- The main procedure.  Fetch all parameters, decode the option
# string, set up the control structure, and finally call process_template
# to expand the module template and process the help text for each module.

procedure t_help()

int	ncols, nlines, list, out_device
long	fi[LEN_FINFO], db_ctime
char	helpdb[SZ_HELPDB], device[SZ_FNAME]
bool	output_is_not_redirected, file_template
pointer	sp, ctrl, option, tempbuf, db, tty, fname, tempdev

long	clktime()
pointer	ttyodes(), hdb_open()
bool	strne(), streq(), clgetb()
int	btoi(), stridxs(), finfo(), fntopnb(), fntgfnb(), strdic()
int	clgeti(), get_option(), fstati(), envgets(), envgeti()

data	tty /NULL/, db_ctime /0/
string	s_helpdb "helpdb"
define	forms_ 91

begin
	call smark (sp)
	call salloc (ctrl, LEN_CTRLSTRUCT, TY_STRUCT)
	call salloc (option, SZ_FNAME, TY_CHAR)
	call salloc (tempbuf, SZ_TEMPBUF, TY_CHAR)
	call salloc (tempdev, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# If we were called without any arguments, do not query for the
	# template, just set it to null and help will be given for the
	# current package.

	call aclri (Memi[ctrl], LEN_CTRLSTRUCT)
	if (clgeti ("$nargs") == 0) {
	    H_OPTION(ctrl) = O_MENU
	    H_TEMPLATE(ctrl) = EOS
	    H_PARNAME(ctrl) = EOS
	    H_SECNAME(ctrl) = EOS
	} else
	    call clgstr ("template", H_TEMPLATE(ctrl), SZ_LINE)

	# get the output device type.  If the device is 'gui' pass off to
	# the XHELP code to execute the GUI, otherwise process the device
	# type once we set up the normal HELP task structure.

	call aclrc (Memc[tempdev], SZ_TEMPBUF)
	call clgstr ("device", Memc[tempdev], SZ_FNAME)
        out_device = strdic (Memc[tempdev], device, SZ_FNAME, HF_DEVICES)
	if (out_device == HF_GUI) {
	    call xhelp (H_TEMPLATE(ctrl))
	    call sfree (sp)
	    return
	}

	# Determine whether the template is a module name template or a
	# file matching template.

	file_template = clgetb ("file_template")
	if (file_template) {
	    H_OPTION(ctrl) = O_HELP
	    goto forms_
	}

	# Fetch the name of the help database, i.e., "helpdb", "helpdir",
	# or the name of a file.  The first time the process runs we open
	# and read in the database.  The database remains in memory between
	# calls to HELP, provided the process does not shutdown, provided
	# the name of the database to be used does not change, and provided
	# a new help database is not created.

	call aclrc (Memc[tempbuf], SZ_TEMPBUF)
	call clgstr (s_helpdb, Memc[tempbuf], SZ_TEMPBUF)
	if (streq (Memc[tempbuf], s_helpdb))
	    if (envgets (s_helpdb, Memc[tempbuf], SZ_TEMPBUF) <= 0)
		call syserrs (SYS_ENVNF, s_helpdb)

	# Check to see if any of the files in the list are newer than the
	# time of the last hdb_open.

	if (db_ctime > 0) {
	    list = fntopnb (Memc[tempbuf], YES)
	    while (fntgfnb (list, Memc[fname], SZ_PATHNAME) != EOF) {
		if (finfo (Memc[fname], fi) == ERR) {
		    call eprintf ("Cannot access help database file `%s'\n")
			call pargstr (Memc[fname])
		} else if (db != NULL && FI_CTIME(fi) > db_ctime) {
		    call hdb_close (db)
		    db = NULL
		    break
		}
	    }
	    call fntclsb (list)
	} else
	    db = NULL

	# Reopen the help database if in-core copy is out of date.
	if (db == NULL || strne (Memc[tempbuf], helpdb)) {
	    call strcpy (Memc[tempbuf], helpdb, SZ_HELPDB)
	    if (db != NULL)
		call hdb_close (db)
	    db = hdb_open (helpdb)
	    db_ctime = clktime (long(0))
	}

	# Fetch the value of the ALL switch.  This determines whether help
	# will stop after processing the first module matching the template,
	# or process all modules in the database which match the template.
	# Explicit use of a pattern matching character anywhere in the template
	# enable allmodoules.

	if (stridxs ("*?[],", H_TEMPLATE(ctrl)) > 0)
	    H_ALLMODULES(ctrl) = YES
	else
	    H_ALLMODULES(ctrl) = btoi (clgetb ("all"))
	
	# If the FILTER_INPUT flag is set, only part of the input text will be
	# processed.  Filtering is only done if printing a single section or
	# parameter.

	H_FILTER_INPUT(ctrl) = NO

	# Determine whether or not text for a single section or parameter
	# is to be output.  If the value of one of these strings is "all",
	# all sections or all parameters are to be output.  If the "all"
	# default is in effect, null the string as a flag to lower level
	# code that all help text is to be processed.

	if (H_OPTION(ctrl) == NULL) {
	    call clgstr ("section", H_SECNAME(ctrl), SZ_SECNAME)
	    if (streq (H_SECNAME(ctrl), "all")) {
		H_SECNAME(ctrl) = EOS
		call clgstr ("parameter", H_PARNAME(ctrl), SZ_PARNAME)
		if (streq (H_PARNAME(ctrl), "all"))
		    H_PARNAME(ctrl) = EOS
	    }
	    if (H_SECNAME(ctrl) != EOS || H_PARNAME(ctrl) != EOS)
		H_FILTER_INPUT(ctrl) = YES
	}

	# Fetch and decode option string; abbreviations are permitted.
	if (H_OPTION(ctrl) != O_MENU) {
	    call clgstr ("option", Memc[option], SZ_FNAME)
	    call strlwr (Memc[option])
	    iferr (H_OPTION(ctrl) = get_option (Memc[option])) {
		call erract (EA_WARN)
		H_OPTION(ctrl) = O_HELP
	    }
	}
forms_
	# Pause between screens of output text only if the standard output
	# is not redirected, and if enabled by the user.

	H_IN(ctrl) = ERR
	H_OUT(ctrl) = STDOUT
	H_NLINES(ctrl) = -1
	H_STATE(ctrl) = BOF
	H_EOF(ctrl) = NO
	H_QUIT(ctrl) = NO

	# If the standard output is not redirected, i.e., if writing to the
	# terminal, determine whether or not output is to be paginated (pause
	# between pages).  If output is redirected, the pagination flag
	# and help option controls whether or not manpage style output is
	# enabled.  Manpage output formatting is desirable only when formatting
	# help text or printing named files.

	H_RAWOUT(ctrl) = NO
	H_MANPAGE(ctrl) = NO
	H_PAGINATE(ctrl) = NO
	output_is_not_redirected = (fstati (STDOUT, F_REDIR) == NO)

	if (out_device < HF_GUI) {
	    if (output_is_not_redirected && out_device == HF_TERMINAL) {
	        if (clgetb ("page")) {
		    H_PAGINATE(ctrl) = YES
		    call xttysize (ncols, nlines)
	        }
	    } else {
	        if (clgetb ("page")) {
		    switch (H_OPTION(ctrl)) {
		    case O_HELP, O_SOURCE, O_SYSDOC, O_ALLDOC:
		        H_MANPAGE(ctrl) = YES
		        H_NLPP(ctrl) = clgeti ("nlpp")
		        call man_init()
		    default:
		        H_RAWOUT(ctrl) = YES
		    }
	        } else
		    H_RAWOUT(ctrl) = YES
	    }
	}

	# Get the current package.  Normally this is specified as the null
	# string or "AskCL", which causes a runtime query of the CL for the
	# current package when a help template is expanded.  The reason for
	# providing this parameter is to allow this query to be disabled
	# (by setting the value to the name of an actual package such as
	# system or clpackage) so that, e.g., the help task can be called
	# as a host level task.

	iferr (call clgstr ("curpack", H_CURPACK(ctrl), SZ_CURPACK))
	    call strcpy ("AskCL", H_CURPACK(ctrl), SZ_CURPACK)

	# Get output device (normally "terminal"), open TTY descriptor for
	# the device.  The TTY descriptor is left open between calls to HELP
	# provided the process does not shutdown and the device name or page
	# size does not change.

	if (out_device == HF_TERMINAL) {
	    if (tty == NULL || strne (Memc[tempdev], device)) {
                call strcpy (Memc[tempdev], device, SZ_FNAME)
	        if (tty != NULL)
	            call ttycdes (tty)
	        tty = ttyodes (device)
	    }
	}

	if (out_device == HF_HTML) {
	    H_FORMAT(ctrl) = HF_HTML
	    H_RAWOUT(ctrl) = YES
	    H_SOFLAG(ctrl) = NO
	    H_MANPAGE(ctrl) = NO
	    tty = NULL
	} else if (out_device == HF_PS || out_device == HF_POSTSCRIPT) {
	    H_FORMAT(ctrl) = HF_PS
	    H_RAWOUT(ctrl) = YES
	    H_SOFLAG(ctrl) = NO
	    H_MANPAGE(ctrl) = NO
	    tty = NULL
	} else if (out_device == HF_TEXT) {
	    H_FORMAT(ctrl) = HF_TEXT
	    H_SOFLAG(ctrl) = NO
	} else {
	    H_FORMAT(ctrl) = HF_TEXT
	    H_SOFLAG(ctrl) = YES
	}
	H_TTY(ctrl) = tty

	# Get left and right margins for output text.  Make sure right margin
	# does not exceed screen width of output device.

	if (H_FORMAT(ctrl) == HF_TEXT) {
	    H_LMARGIN(ctrl) = max (1, clgeti ("lmargin"))
	    H_RMARGIN(ctrl) = max (H_LMARGIN(ctrl) + 1, clgeti ("rmargin"))
	    if (streq (device, "terminal"))
	        H_RMARGIN(ctrl) = min (H_RMARGIN(ctrl), envgeti ("ttyncols"))
	}

	# Initialization is completed, control structure is completed.
	# Format and output the help text.  If we have a module name template
	# process the template against the help database, otherwise work
	# directly out of the named files.

	if (file_template)
	    call do_file_template (H_TEMPLATE(ctrl), ctrl)
	else
	    call do_module_template (db, H_TEMPLATE(ctrl), ctrl)

	# Finish last page and return buffer space if writing manpages.
	if (H_MANPAGE(ctrl) == YES)
	    call man_close (H_OUT(ctrl))

	call sfree (sp)
end
