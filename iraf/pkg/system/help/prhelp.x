# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"help.h"
include	"helpdir.h"

.help print_help
.nf __________________________________________________________________________
PRINT_HELP -- Print help documentation for the named module or parameter.
We are called with the name of a single module; all fiddling with packages
and templates has been performed by the time we are called.  The help
directory is open and contains the names of the files containing the help
source for the module.  Our main task is to determine what kind of help
is desired and call the appropriate routine.

Recall that the principal options are

        option                    meaning                  file

	help		print help block for module	   hlp
	param		print help for single param	   hlp
	section		print a single section		   hlp
	files	        print file names		   ...
	source		print source file		   src
	sysdoc		print system documentation	   sys
	alldoc		print all documentation		 hlp,sys
	summary		print help block titles		 hlp,sys
.endhelp ______________________________________________________________________

define	exit_		90


# PRINT_HELP -- Print the indicated type of help text for the named module.
# If we are called the module exists in the package, but the type of help
# information indicated may not exist.  Return OK if help is printed, ERR
# otherwise.

int procedure print_help (db, hp, module, modnum, ctrl)

pointer	db			# help database descriptor
pointer	hp			# help directory for package
char	module[ARB]		# name of module for which help is desired
int	modnum			# module number within package directory
pointer	ctrl			# help control parameters

bool	not_found
int	option
pointer	sp, fname, pakname, modname
int	hd_getname()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (pakname, SZ_FNAME, TY_CHAR)
	call salloc (modname, SZ_FNAME, TY_CHAR)

	# Handle options which do not access a help file.
	option = H_OPTION(ctrl)
	H_STATE(ctrl) = BOF
	not_found = true

	if (option == O_MENU) {
	    if (hd_getname (hp, modnum, TY_MEN, Memc[fname], SZ_PATHNAME) == 0)
		goto exit_
	    iferr (call pr_file (Memc[fname], ctrl, "test"))
		call erract (EA_WARN)
	    else
		not_found = false

	} else if (option == O_FILES) {
	    # Print names of all files associated with module.
	    ifnoerr (call pr_filenames (hp, module, modnum, ctrl))
		not_found = false

	} else if (option == O_DIR) {
	    # Print a directory of all help blocks for the named package.
	    ifnoerr (call pr_directory (db, hp, module, modnum, ctrl))
		not_found = false

	} else if (option == O_SOURCE) {
	    # Print source file, if given.
	    if (hd_getname (hp, modnum, TY_SRC, Memc[fname], SZ_PATHNAME) == 0)
		goto exit_
	    iferr (call pr_file (Memc[fname], ctrl, "test"))
		call erract (EA_WARN)
	    else
		not_found = false

	} else if (option == O_SUMMARY) {
	    # Scan hlp file and print summary of help blocks.
	    if (hd_getname (hp, modnum, TY_HLP, Memc[fname], SZ_PATHNAME) == 0)
		goto exit_
	    iferr (call pr_summary (Memc[fname], ctrl))
		call erract (EA_WARN)
	    else
		not_found = false

	} else {
	    # Get the package and module names.  If there is no package name
	    # in the help directory, it is root help directory, and the module
	    # name is actually the name of a package.

	    if (HD_PAKNAME(hp) == 0) {
		call strcpy (module, Memc[pakname], SZ_FNAME)
		Memc[modname] = EOS
	    } else {
		call strcpy (Memc[HD_SBUF(hp) + HD_PAKNAME(hp)],
		    Memc[pakname], SZ_FNAME)
		call strcpy (module, Memc[modname], SZ_FNAME)
	    }

	    # Print full helpblock (default action).
	    if (option == O_HELP || option == O_ALLDOC ||
		option == O_REFERENCES) {

		if (hd_getname (hp,modnum,TY_HLP,Memc[fname],SZ_PATHNAME) > 0) {
		    ifnoerr {
			call pr_helpblock (Memc[fname],
			    Memc[pakname], Memc[modname], TY_HLP, ctrl)
		    } then
			not_found = false
		}
	    }

	    # Print system documentation.
	    if (option == O_SYSDOC || option == O_ALLDOC) {
		if (hd_getname (hp,modnum,TY_SYS,Memc[fname],SZ_PATHNAME) > 0) {
		    ifnoerr {
			call pr_helpblock (Memc[fname],
			    Memc[pakname], Memc[modname], TY_HLP, ctrl)
		    } then
			not_found = false
		}
	    }
	}

exit_
	call flush (H_OUT(ctrl))
	call sfree (sp)
	if (not_found)
	    return (ERR)
	else
	    return (OK)
end
