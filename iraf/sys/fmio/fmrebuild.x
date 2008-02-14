# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FM_REBUILD -- Rebuild a datafile.  This has no affect on the logical content
# of a datafile, but is desirable for efficiency reasons to eliminate waste
# space (e.g., from deleted lfiles), and render the file structures logically
# contiguous within the file, increasing i/o access efficiency.

procedure fm_rebuild (dfname)

char	dfname[ARB]		#I datafile name

pointer	sp, tfname
errchk	fm_copy, fm_delete, fm_rename

begin
	call smark (sp)
	call salloc (tfname, SZ_PATHNAME, TY_CHAR)

	# The copy operation rebuilds a datafile.
	call mktemp (dfname, Memc[tfname], SZ_PATHNAME)
	call fm_copy (dfname, Memc[tfname])
	call fm_delete (dfname)
	call fm_rename (Memc[tfname], dfname)

	call sfree (sp)
end
