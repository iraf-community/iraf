# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"help.h"
include	"helpdir.h"

# PR_FILENAMES -- Print file names associated with module.

define	NFILES	5

procedure pr_filenames (hp, module, modnum, ctrl)

pointer	hp
char	module[ARB]
int	modnum
pointer	ctrl

int	i, nfiles, ftype[NFILES]
pointer	sp, lbuf, pakname, modname, fname[NFILES]
int	hd_getname()
errchk	hd_getname, houtput
data	ftype /TY_HLP, TY_SYS, TY_SRC, TY_PKG, TY_MEN/
string	fcode "hlpsyssrcpkgmen"


begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (pakname, SZ_FNAME, TY_CHAR)
	call salloc (modname, SZ_FNAME, TY_CHAR)

	do i = 1, NFILES
	    call salloc (fname[i], SZ_PATHNAME, TY_CHAR)

	nfiles = 0
	do i = 1, NFILES
	    if (hd_getname (hp, modnum, ftype[i], Memc[fname[i]],
	    SZ_PATHNAME) > 0)
		nfiles = nfiles + 1

	if (nfiles > 0) {
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

	    # Print "pakname.modname".
	    if (H_LENTL(ctrl) > 1)
		call pr_modname (ctrl, Memc[pakname], Memc[modname])

	    # List the names of the help files.
	    do i = 1, NFILES
		if (Memc[fname[i]] != EOS) {
		    call sprintf (Memc[lbuf], SZ_LINE, "\t%0.3s = %s\n")
			call pargstr (fcode[(i-1)*3+1])
			call pargstr (Memc[fname[i]])
		    call houtput (ctrl, Memc[lbuf])
		}
	}

	call sfree (sp)
end
