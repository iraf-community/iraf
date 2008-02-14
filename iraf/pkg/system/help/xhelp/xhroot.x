# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"xhelp.h"


# XH_ROOT_PKG -- Make the root package.  Search the help database and
# create a package list for all modules found.  We add special entries for
# system modules (imfort/math/os) which are not normally in the help tree
# but provide documents.

procedure xh_root_pkg (xh)

pointer	xh				#i struct pointer.

size_t	sz_val
pointer sp, fname, buf, ip, op, lp, list

int     xh_pkglist()
int     gstrcpy(), fntgfnb()
pointer	fntopnb()
bool    strne()

begin
        call smark (sp)
        sz_val = SZ_FNAME
        call salloc (fname, sz_val, TY_CHAR)
        call salloc (buf, sz_val, TY_CHAR)

        # Set initial packages and help databases.  This consists of the
        # system documentation (in sys$sys.hd), the contents of the
        # clpackage module and each of the external packages.

        lp = XH_LPTR(xh)

        # Create an entry for seldom-read system docs.
	if (XH_SHOWTYPE(xh) == YES) {
            lp = lp + gstrcpy ("imfort. ", Memc[lp], ARB)
            lp = lp + gstrcpy ("math. ", Memc[lp], ARB)
            lp = lp + gstrcpy ("os. ", Memc[lp], ARB)
	} else {
            lp = lp + gstrcpy ("imfort ", Memc[lp], ARB)
            lp = lp + gstrcpy ("math ", Memc[lp], ARB)
            lp = lp + gstrcpy ("os ", Memc[lp], ARB)
	}

        # Add the external packages to the list.
        list = fntopnb (HELPDB(xh), YES)
        while (fntgfnb (list, Memc[fname], SZ_FNAME) != EOF) {
            op = buf
            ip = fname
            while (Memc[ip] != '$' && Memc[ip] != EOS && Memc[ip] != ',') {
                Memc[op] = Memc[ip]
                ip = ip + 1
                op = op + 1
            }
            Memc[op] = EOS
            if (strne(Memc[buf],"lib")) {
                lp = lp + gstrcpy (Memc[buf], Memc[lp], ARB)
		if (XH_SHOWTYPE(xh) == YES)
                    lp = lp + gstrcpy (".", Memc[lp], ARB)
                lp = lp + gstrcpy (" ", Memc[lp], ARB)
            }
        }

        # Add the clpackage contents to the list.
        lp = lp + xh_pkglist (xh, "clpackage", HELPDB(xh), Memc[lp])

	if (lp > (XH_LPTR(xh) + SZ_HELPLIST))
	    call error (1, "Memory error: LIST pointer overflow.")

        # Sort the list so it's presentable.
        call xh_sort_list (LIST(xh))

	call sfree (sp)
end
