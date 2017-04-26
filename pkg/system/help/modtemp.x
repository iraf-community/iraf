# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<syserr.h>
include	"help.h"

# DO_MODULE_TEMPLATE -- Called with a template defining the packages and
# modules for which help is desired, and the control parameters defining
# the type of help desired.  Expand the template into a list of packages
# and modules and process the help for each module.
# 
# Most of the work here is done by HD_LOAD, which opens and interprets a
# help directory, and TL_OPEN/TL_READ, which expand the template list.
# Each element of the template list is a module matching pattern for a
# single package.  When this is expanded we get the names of the individual
# modules and call PRINT_HELP to print the help text for a single module
# of a particular package.  In the worst case (template "*.*"), we might
# process all modules in the help database.

procedure do_module_template (db, tlist, ctrl)

pointer	db			# help database to be used
char	tlist[ARB]		# template list ("a.*,b.*,...")
pointer	ctrl			# Help control structure

int	m, pk
bool	no_matches, matchall
pointer	sp, fname, template, module, pakname, modtemp, curmod
pointer	hp_sys, hp_pak, tl, ip, op, modlist

bool	strne()
int	tl_read(), ml_read(), print_help()
int	stridxs(), ml_patmatch(), hd_findmod(), hd_getname()
pointer	hdb_load(), tl_open(), ml_open()
errchk	tl_read, hd_findmod, hd_getname, ml_read, hdb_load
errchk	tl_open, ml_open, print_help

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (template, SZ_LINE, TY_CHAR)
	call salloc (module, SZ_FNAME, TY_CHAR)
	call salloc (pakname, SZ_FNAME, TY_CHAR)
	call salloc (modtemp, SZ_FNAME, TY_CHAR)
	call salloc (curmod, SZ_FNAME, TY_CHAR)

	# The help database contains the package names and references to
	# all help modules in the package.  Load the help database index
	# ("_index") which contains a list of all packages in the database.

	hp_sys = hdb_load (db, "_index")

	# Expand the template list into a list of simple "pak.mod"
	# templates, where the package name is spelled out and only
	# the module template remains to be expanded.  The null
	# template list gets turned into "curpack.".

	tl = tl_open (db, hp_sys, tlist, ctrl)
	Memc[curmod] = EOS
	no_matches = true
	matchall = (H_ALLMODULES(ctrl) == YES)

	# Process each "package_name.module_template" element of the TL
	# list.  The package_name field is always valid.  If the module
	# field is null, package help is desired, so print help on the
	# package as if it were a module.  Otherwise, expand the module
	# template for the package and process help on each module.

	while (tl_read (tl, Memc[template], SZ_FNAME) != EOF) {
	    # Extract package name.
	    op = pakname
	    for (ip=template;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    for (;  Memc[ip] != '.' && Memc[ip] != EOS;  ip=ip+1) {
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	    Memc[op] = EOS

	    # Extract module template.
	    op = modtemp
	    if (Memc[ip] == '.')
		ip = ip + 1
	    for (;  Memc[ip] != EOS;  ip=ip+1) {
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	    Memc[op] = EOS

	    # The following is used to count the number of matches for a module
	    # (used to generate the "No help available" message).  The
	    # template list will typically contain sequences like "curpak.mod",
	    # "paki.", "pakj.", "pak1.mod", "pak2.mod", and so on (where the
	    # pak[ij] are the full names of packages matched by an ambiguous
	    # template).  Reset no_matches whenever the module name changes.
	    # If the module name disappears, i.e., if the template is a package
	    # name, then we have a match if the old module name is an
	    # abbreviation for the package.

	    if (Memc[curmod] == EOS || strne(Memc[modtemp],Memc[curmod])) {
		if (Memc[modtemp] == EOS) {
		    if (ml_patmatch (Memc[pakname], Memc[modtemp]) > 0)
			no_matches = false
		} else {
		    if (no_matches && Memc[curmod] != EOS) {
			call eprintf ("No help available for `%s'\n")
			    call pargstr (Memc[curmod])
		    }
		    no_matches = true
		    call strcpy (Memc[modtemp], Memc[curmod], SZ_FNAME)
		}
	    }

	    # Search system help directory for the named package.  If it
	    # cannot be found it is not an error if the original template
	    # was ambiguous (did not have a ".").  A package reference is
	    # always generated for an ambiguous template because we do not
	    # know what the user wants help for.

	    pk = hd_findmod (hp_sys, Memc[pakname])
	    if (pk == 0) {
		if (Memc[modtemp] == EOS)
		    if (stridxs (".", tlist) != 0) {
			call eprintf ("Package `%s' not in Help database\n")
			    call pargstr (Memc[pakname])
		    }
		next
	    }

	    # If module template is null, user wants help on the package
	    # itself; print help on the package as a module of hp_sys.
	    # Otherwise expand the module template and print help on each
	    # module.

	    if (Memc[modtemp] == EOS) {
		if (print_help (db, hp_sys, Memc[pakname], pk, ctrl) == ERR) {
		    call eprintf ("No help available for package `%s'\n")
			call pargstr (Memc[pakname])
		}
	    } else {
		# Get name of package helpdir for package `pk'.
		if (hd_getname (hp_sys,pk,TY_PKG,Memc[fname],SZ_FNAME) == 0) {
		    call eprintf ("No help available for package `%s'\n")
			call pargstr (Memc[pakname])
		    next
		}

		# Open the help directory for the package, then open the
		# template.

		iferr (hp_pak = hdb_load (db, Memc[fname]))
		    next
		modlist = ml_open (hp_pak, Memc[modtemp])
	
		# Process each module matching the module template.  The QUIT
		# flag is set if the user responds no to the "more" query
		# between help blocks or files.

		while (ml_read (modlist, m, Memc[module], SZ_FNAME) != EOF) {
		    if (print_help (db, hp_pak, Memc[module], m, ctrl) == OK) {
			no_matches = false
			if (!matchall)
			    break
		    }
		    if (H_QUIT(ctrl) == YES)
			break
		}

		call ml_close (modlist)
		call hdb_free (db, hp_pak)
	    }

	    if (H_QUIT(ctrl) == YES)
		break
	    else if (!matchall && !no_matches)
		break
	}

	# A final check for no matches is necessary for the last module in
	# the template list.

	if (no_matches && Memc[curmod] != EOS) {
	    call eprintf ("No help available for `%s'\n")
		call pargstr (Memc[curmod])
	}

	call tl_close (tl)
	call hdb_free (db, hp_sys)
	call sfree (sp)
end
