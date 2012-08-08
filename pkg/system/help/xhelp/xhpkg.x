# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<error.h>
include	"../help.h"
include	"../helpdir.h"
include	"xhelp.h"


# XH_GPKGLIST -- Get the requested package list as a sorted array of pointers.
# This is essentially a "help <pkg>" request,  the caller passes the sorted
# list returned in 'pkglist" required.

int procedure xh_pkglist (xh, topic, helpdb, pkglist)

pointer xh                		#i task descriptor pointer
char    topic[ARB]                	#i search key
char    helpdb[ARB]             	#i filename of database to be examined
char    pkglist[ARB]             	#o package list

int     i, m
pointer	sp, pknm, hp, pp, sym
pointer db, ixoff, ix

bool    strne()
int     gstrcpy(), strsearch(), hd_getname()
pointer hdb_open(), hdb_load(), stfind()
errchk  hdb_open, hdb_printpack, hdb_load

begin
        call smark (sp)
        call salloc (pknm, MAX_MENUSIZE, TY_POINTER)

        db = hdb_open (helpdb)
        ixoff = HDB_INDEXPTR(db)
	pp = NULL

	# Search for the right topic.
        do i = 1, HDB_NENTRIES(db) {
            ix = ixoff + (i - 1) * LEN_HDBINDEX
            if (strne (DBI_KEY(ix), "_index") &&
                strsearch (DBI_KEY(ix),topic) != 0) {

	        iferr (hp = hdb_load (db, DBI_KEY(ix))) {
      	            call sfree (sp)
      	            call erract (EA_WARN)
                    return (0)
        	}

		# If this isn't the package we're after then move on.
        	if (HD_PAKNAME(hp) == 0 ||
		    strne (topic, Memc[HD_SBUF(hp)+HD_PAKNAME(hp)]))
			next

	        # Extract the names of the modules in the package.  Save the
	        # pointers in an array for the table print routine.

		pp = 1
	        for (m=0;  m < MAX_MENUSIZE;  m=m+1) {
	            call salloc (Memi[pknm+m], MAX_NAMELEN, TY_CHAR)
	            if (hd_getname (hp, m+1, TY_MODNAME, Memc[Memi[pknm+m]],
	                MAX_NAMELEN) <= 0)
                    	    break

		    # Copy the names to the output array.
		    pp = pp + gstrcpy (Memc[Memi[pknm+m]], pkglist[pp], ARB)
		    if (XH_SHOWTYPE(xh) == YES && XH_STP(xh) != NULL) {
		        sym = stfind (XH_STP(xh), Memc[Memi[pknm+m]])
		        if (sym != NULL)
		            pp = pp + gstrcpy (".", pkglist[pp], ARB)
		    }
		    pp = pp + gstrcpy (" ", pkglist[pp], ARB)
        	}
		break
            }
        }
	if (pp != NULL)
	    pkglist[pp] = EOS

        call hdb_free (db, hp)
        call hdb_close (db)
        call sfree (sp)
	return (pp)
end


# XH_PKGPATH -- Get the package path associated with a particular task.
# If we're given a parent package follow it back so we get the correct
# path for a task that may be defined multiple places (e.g. SPLOT).

procedure xh_pkgpath (xh, topic, curpack, path)

pointer xh                                      #i task struct pointer
char    topic[ARB]                           	#i help topic
char    curpack[ARB]                           	#i help topic
char    path[ARB]                           	#o package path

pointer	sp, pkg, task, buf
int	strncmp(), xh_pkgname()
bool	streq()

begin
	call smark (sp)
	call salloc (pkg, SZ_FNAME, TY_CHAR)
	call salloc (task, SZ_FNAME, TY_CHAR)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	if (curpack[1] == EOS || 
	    streq (topic, curpack) ||
	    strncmp ("root", curpack, 4) == 0 || 
	    streq ("clpackage", curpack)) {
	        call strcpy (topic, Memc[task], SZ_FNAME)
	        call strcpy (topic, path, SZ_FNAME)
	} else {
	    call strcpy (curpack, Memc[task], SZ_FNAME)
	    call sprintf (path, SZ_PATHNAME, "%s.%s")
		call pargstr (curpack)
		call pargstr (topic)
	}

	Memc[pkg] = EOS
	while (xh_pkgname (xh, Memc[task], Memc[pkg]) == OK) {
	    if (strncmp ("root", Memc[pkg], 4) == 0 || 
		streq (Memc[task], Memc[pkg]) ||
		streq ("clpackage", Memc[pkg]))
		    break
	    else {
	        call sprintf (Memc[buf], SZ_PATHNAME, "%s.%s")
		    call pargstr (Memc[pkg])
		    call pargstr (path)
		call strcpy (Memc[buf], path, SZ_PATHNAME)
	    }
	    call strcpy (Memc[pkg], Memc[task], SZ_FNAME)
	    Memc[pkg] = EOS
	}

	call sfree (sp)
end


# XH_PKGNAME -- Get the package name associated with a particular task.

int procedure xh_pkgname (xh, topic, pack)

pointer xh                                      #i task struct pointer
char    topic[ARB]                           	#i help topic
char    pack[ARB]                           	#o package

pointer sp, line, fname
long	fsize, fstatl()
int	fd, status, getline(), open(), stridxs()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_LINE, TY_CHAR)

	status = ERR

	# Get a temp file name.
	call mktemp ("tmp$xhelpi", Memc[fname], SZ_FNAME)

	# Open a temp file with the help information found.
	fd = open (Memc[fname], NEW_FILE, TEXT_FILE)
	call xh_get_help (fd, topic, "", "", HF_HTML, HELPDB(xh),
	    "all", "files")
	call close (fd)

	# Open the results file for reading.
	fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	fsize = fstatl (fd, F_FILESIZE)

	# Search the results for the package line.
        if (fsize != 0) {
	    status = OK
            while (getline (fd, Memc[line]) != EOF) {

	        # Extract the package name.
	        if (stridxs (":", Memc[line]) > 0) {
		    Memc[line+stridxs(".",Memc[line])-1] = EOS
		    call strcpy (Memc[line], pack, SZ_FNAME)
	            break
	        }
	    }
	}

	call close (fd)				# clean up 
	call delete (Memc[fname])
	call sfree (sp)

	return (status)
end
