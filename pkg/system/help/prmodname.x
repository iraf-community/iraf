# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"help.h"

# PR_MODNAME -- Print the module name header "pakname.modname".  Omit the
# package name if it begins with an underscore, unless there is no module
# name.

procedure pr_modname (ctrl, pakname, modname)

pointer	ctrl			# help control parameters
char	pakname[ARB]		# package name
char	modname[ARB]		# module name

pointer	sp, lbuf

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	if (pakname[1] != EOS || modname[1] != EOS) {
	    call houtput (ctrl, "\n")
	    if (pakname[1] == '_' && modname[1] != EOS) {
		call sprintf (Memc[lbuf], SZ_LINE, "%s:\n")
		    call pargstr (modname)
	    } else {
		call sprintf (Memc[lbuf], SZ_LINE, "%s.%s:\n")
		    call pargstr (pakname)
		    call pargstr (modname)
	    }
	    call houtput (ctrl, Memc[lbuf])
	}

	call sfree (sp)
end
