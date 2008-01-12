# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<finfo.h>
include	<error.h>
include	"../help.h"
include	"xhelp.h"


# XH_HELP -- Get the requested help topic and send the result to the GUI.

procedure xh_help (xh, topic, curpack, opt)

pointer xh                                      #i task struct pointer
char    topic[ARB]                           	#i help topic
char    curpack[ARB]                           	#i current package
char    opt[ARB]                           	#i help option

pointer helpstr
int	ip, fdi
long	fsize
char	ch, fname[SZ_FNAME], err[SZ_LINE]

long	fstatl()
char	getc()
int	open()
bool	strne()

begin
	# Get a temp file name.
	call mktemp ("tmp$xhelpi", fname, SZ_FNAME)

	# Open a temp file with the help information found.
	fdi = open (fname, NEW_FILE, TEXT_FILE)
	call xh_get_help (fdi, topic, curpack, "", HF_HTML, HELPDB(xh),
	    "all", opt)
	call close (fdi)

	# Open the results file for reading.
	fdi = open (fname, READ_ONLY, TEXT_FILE)
	fsize = fstatl (fdi, F_FILESIZE)

	# If no results try using the topic name as a curpack param.
	if (fsize == 0) {
	    call close (fdi)		# clean up from before
	    call delete (fname)

	    # Open a temp file with the help information found.
	    fdi = open (fname, NEW_FILE, TEXT_FILE)
	    call xh_get_help (fdi, topic, topic, "", HF_HTML, HELPDB(xh),
		"all", opt)
	    call close (fdi)

	    # Open the results file for reading.
	    fdi = open (fname, READ_ONLY, TEXT_FILE)
	    fsize = fstatl (fdi, F_FILESIZE)

	    # If we still have nothing then punt...
	    if (fsize == 0 && topic[1] != EOS) {
	        if (strne (opt, "help")) {
	            call sprintf (err, SZ_LINE,
		        "No '%s' option help available\n for `%s'.")
		            call pargstr (opt)
		            call pargstr (topic)
	        } else {
	            call sprintf (err, SZ_LINE, "No help available for\n`%s'.")
		        call pargstr (topic)
	        }
	        call gmsg (XH_GP(xh), "alert", err)
	        call close (fdi)
	        call delete (fname)
	        return
	    }
	}

	# Now filter the file to escape the curly braces so they pass thru
	# to the Tcl cleanly.  Put the result in the string sent to the GUI.
	call calloc (helpstr, fsize + SZ_LINE, TY_CHAR)
	ip = helpstr
	repeat {
	    ch = getc (fdi, ch)
	    if (ch == '{' || ch == '}') {
	        Memc[ip] = '\\'
		ip = ip + 1
	    }
	    Memc[ip] = ch
	    ip = ip + 1
	} until (ch == EOF)
	Memc[ip-1] = EOS

	# Clean up.
	call close (fdi)
	call delete (fname)

	# Send the help text to the GUI who will display it.
	call gmsg (XH_GP(xh), "helpres", Memc[helpstr])
	call mfree (helpstr, TY_CHAR)
end


# XH_GET_HELP -- The main work procedure, i.e. a rip-off of the t_help()
# procedure.  Decode the option string, set up the control structure, and 
# finally call process_template to expand the module template and process
# the help text for each module.  The output is written to a temp file
# opened by the calling procedure which may optionally sort it or display
# it as is.

procedure xh_get_help (fd, topic, curpack, file, format, helpdb, section, opt)

int	fd					#i file descriptor of result
char	topic[ARB]				#i topic
char    curpack[ARB]                           	#i current package
char    file[ARB]                           	#i file template
int	format					#i output format (text|html|ps)
char	helpdb[ARB]				#i help database
char	section[ARB]				#i section on which to get help
char	opt[ARB]				#i type of help

int	list
long	fi[LEN_FINFO], db_ctime
pointer	sp, ctrl, optn, db, fname

long	clktime()
pointer	hdb_open()
bool	strne(), streq()
int	stridxs(), finfo(), fntopnb(), fntgfnb(), get_option()

errchk	hdb_open

data	db_ctime /0/
define	forms_ 91

begin
	call smark (sp)
	call salloc (ctrl, LEN_CTRLSTRUCT, TY_STRUCT)
	call salloc (optn, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# If we were called without any arguments, do not query for the
	# template, just set it to null and help will be given for the
	# current package.

	call aclri (Memi[ctrl], LEN_CTRLSTRUCT)
	if (topic[1] == EOS) {
	    if (file[1] == EOS) {
	        H_OPTION(ctrl) = O_MENU
	        H_TEMPLATE(ctrl) = EOS
	        H_PARNAME(ctrl) = EOS
	        H_SECNAME(ctrl) = EOS
	    } else
	        call strcpy (file, H_TEMPLATE(ctrl), SZ_LINE)
	} else {
	    call strcpy (topic, H_TEMPLATE(ctrl), SZ_LINE)
	}


	# Check to see if any of the files in the list are newer than the
	# time of the last hdb_open.  The first time the process runs we open
	# and read in the database.  The database remains in memory between
	# calls to HELP, provided the process does not shutdown, provided
	# the name of the database to be used does not change, and provided
	# a new help database is not created.

	if (db_ctime > 0) {
	    list = fntopnb (helpdb, YES)
	    while (fntgfnb (list, Memc[fname], SZ_PATHNAME) != EOF) {
                if (finfo (Memc[fname], fi) != ERR) {
		    if (db != NULL && FI_CTIME(fi) > db_ctime) {
		        call hdb_close (db)
		        db = NULL
		        break
		    }
		}
	    }
	    call fntclsb (list)
	} else
	    db = NULL

	# Reopen the help database if in-core copy is out of date.
	if (db == NULL) {
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
	    H_ALLMODULES(ctrl) = NO
	
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
	    call strcpy (section, H_SECNAME(ctrl), SZ_SECNAME)
	    if (streq (H_SECNAME(ctrl), "all")) {
		H_SECNAME(ctrl) = EOS
		H_PARNAME(ctrl) = EOS
	    }
	    if (H_SECNAME(ctrl) != EOS || H_PARNAME(ctrl) != EOS)
		H_FILTER_INPUT(ctrl) = YES
	}

	# Fetch and decode option string; abbreviations are permitted.
	if (H_OPTION(ctrl) != O_MENU) {
	    call strcpy (opt, Memc[optn], SZ_FNAME)
	    call strlwr (Memc[optn])
	    iferr (H_OPTION(ctrl) = get_option (Memc[optn])) {
		H_OPTION(ctrl) = O_HELP
		call erract (EA_WARN)
	    }
	}

forms_
	# Pause between screens of output text only if the standard output
	# is not redirected, and if enabled by the user.

	H_IN(ctrl) = ERR
	H_OUT(ctrl) = fd
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

	H_RAWOUT(ctrl) = YES
	H_MANPAGE(ctrl) = NO
	H_PAGINATE(ctrl) = NO
	H_SOFLAG(ctrl) = NO
	H_FORMAT(ctrl) = format

	# We don't produce output to a screen so shut off the tty.

	H_TTY(ctrl) = NULL

	# Set left and right margins for output text format.

	H_LMARGIN(ctrl) = 1
	H_RMARGIN(ctrl) = 72

	# Copy the current package to the control struct.
	if (strne(curpack,"Home") && strne(curpack,""))
            call strcpy (curpack, H_CURPACK(ctrl), SZ_CURPACK)

	# Initialization is completed, control structure is completed.
	# Format and output the help text.  If we have a module name template
	# process the template against the help database, otherwise work
	# directly out of the named files.

	if (file[1] == EOS)
            call do_module_template (db, H_TEMPLATE(ctrl), ctrl)
	else
            call do_file_template (H_TEMPLATE(ctrl), ctrl)

	call sfree (sp)
end
