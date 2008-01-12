include	<imhdr.h>
include	"gf.h"

# Diagnostic procedures to dump structures used by gflib

# GF_DUMP_CACHE -- Print the database cache

procedure gf_dump_cache (fd)

int	fd		# i: file descriptor
#--
include	"gfdb.com"

int	idb
pointer	im

begin
	call fprintf (fd, "== Database Cache ==\n\n")

	do idb = 1, high {
	    im = imcache[idb]

	    call fprintf (fd, "%2d %s\t%d\t%d\t%2d %3s %3s %3s\n")

	    call pargi (idb)

	    if (im == NULL) {
		call pargstr ("[None]")
	    } else {
		call pargstr (IM_HDRFILE(im))
	    }

	    call pargi (im)
	    call pargi (dbcache[idb])
	    call pargi (gncache[idb])

	    if (excache[idb] == NO) {
		call pargstr ("NO")
	    } else {
		call pargstr ("YES")
	    }

	    if (incache[idb] == NO) {
		call pargstr ("NO")
	    } else {
		call pargstr ("YES")
	    }

	    if (upcache[idb] == NO) {
		call pargstr ("NO")
	    } else {
		call pargstr ("YES")
	    }
	}

	call flush (fd)
end

# GF_DUMP_DB -- Print the keyword database

procedure gf_dump_db (fd, im)

int	fd		# i: file descriptor
pointer	im		# i: image descriptor
#--
pointer	db
int	gf_find_db()

begin
	db = gf_find_db (im, PARAM_DB)

	call fprintf (fd, "== %s keyword db ==\n\n")
	call pargstr (IM_HDRFILE(im))

	if (db == NULL) {
	    call fprintf (fd, "[No database]\n")
	} else {
	    call gf_dumphash (db, fd)
	}

	call flush (fd)
end

# GF_DUMP_DUP -- Dump duplicate keywords in the user area

procedure gf_dump_dup (fd, im)

int	fd		# i: file descriptor
pointer	im		# i: image descriptor
#--
pointer	both

begin
	call fprintf (fd, "== %s duplicates ==\n\n")
	call pargstr (IM_HDRFILE(im))

	call gf_both (im, both)
	call gf_dumphash (both, fd)

	call gf_freehash (both)
	call flush (fd)
end

# GF_DUMP_KEY -- Dump first value of keyword found in user area

procedure gf_dump_key (fd, im, keyword)

int	fd		# i: file descriptor
pointer	im		# i: image descriptor
char	keyword[ARB]	# i: keyword name
#--
int	ic, spool, found
pointer	sp, ua, line, longkey

bool	strne()
int	strlen(), strncmp(), stropen(), getline()

begin
	# Allocate memory for strings

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (longkey, SZ_KEYWORD, TY_CHAR)

	# Pad keyword with blanks

	call strcpy (keyword, Memc[longkey], SZ_KEYWORD)

	do ic = strlen (keyword), SZ_KEYWORD-1
	    Memc[longkey+ic] = ' '

	Memc[longkey+SZ_KEYWORD] = EOS

	# Write title

	call fprintf (fd, "== keyword in %s ==\n")
	call pargstr (IM_HDRFILE(im))

	# Search for keyword in user area

	ua = IM_USERAREA(im)
	spool = stropen (Memc[ua], ARB, READ_ONLY)

	found = NO
	call seek (spool, BOF)

	while (getline (spool, Memc[line]) != EOF) {
	    if (strncmp (Memc[line], Memc[longkey], SZ_KEYWORD) == 0) {
		call putline (fd, Memc[line])
		found = YES

		if (strne (keyword, "HISTORY") && strne (keyword, "COMMENT"))
		    break
	    }
	}

	if (found == NO) {
	    call fprintf (fd, "%s not found in user area\n")
	    call pargstr (keyword)
	}

	call fprintf (fd, "\n")

	# Close up shop

	call strclose (spool)
	call flush (fd)
	call sfree (sp)
end

# GF_DUMP_SPOOL -- Print the contents of a spool file

procedure gf_dump_spool (fd, spool)

int	fd		# i: file descriptor
int	spool		# i: spool file
#--

begin
	if (spool == NULL)
	    return

	call putline (fd, "== spool file ==\n")

	call seek (spool, BOF)
	call fcopyo (spool, fd)
	call seek (spool, BOF)

	call flush (fd)
end

# GF_DUMP_UA -- Print the user area of an image

procedure gf_dump_ua (fd, im)

int	fd		# i: file descriptor
pointer	im		# i: image descriptor
#--
int	ext
pointer	ua

int	stropen ()

begin
	call fprintf (fd, "== user area %s ==\n\n")
	call pargstr (IM_HDRFILE(im))

	ua = IM_USERAREA(im)
	ext = stropen (Memc[ua], ARB, READ_ONLY)

	call fcopyo (ext, fd)

	call strclose (ext)
	call flush (fd)
end

