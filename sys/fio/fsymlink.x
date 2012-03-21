# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<knet.h>

# FSYMLINK -- Remove a symlink.

procedure fsymlink (link, target)

char	link[ARB]			# link name
char	target[ARB]			# target file

int	status
pointer	sp, oslnk, ostgt, lname, tname
int	access()
errchk	syserrs

begin
	call smark (sp)
	call salloc (lname, SZ_PATHNAME, TY_CHAR)
	call salloc (tname, SZ_PATHNAME, TY_CHAR)
	call salloc (oslnk, SZ_PATHNAME, TY_CHAR)
	call salloc (ostgt, SZ_PATHNAME, TY_CHAR)

	# It is an error if the link file already exists.
	if (access (link, 0, 0) == YES)
	    call syserrs (SYS_FSYMLINK, link)
	
        # Always present ZFLINK with a full pathname (rather than an
        # absolute or cwd relative filename), in case the kernel procedure
        # is not smart enough to handle all these possibilities.
	call aclrc (Memc[oslnk], SZ_PATHNAME)
        iferr (call fmapfn (link, Memc[oslnk], SZ_PATHNAME))
            call syserrs (SYS_FSYMLINK, link)

#        call strupk (Memc[oslnk], Memc[oslnk], SZ_PATHNAME)
#        call strpak (Memc[oslnk], Memc[oslnk], SZ_PATHNAME)


	call aclrc (Memc[ostgt], SZ_PATHNAME)
        iferr (call fmapfn (target, Memc[ostgt], SZ_PATHNAME))
            call syserrs (SYS_FSYMLINK, target)
        
#        call strupk (Memc[ostgt], Memc[ostgt], SZ_PATHNAME)
#        call strpak (Memc[ostgt], Memc[ostgt], SZ_PATHNAME)

	# Try to create the symlink.
	call zflink (Memc[ostgt], Memc[oslnk], status)
	if (status == ERR)
	    call syserrs (SYS_FSYMLINK, link)

	call sfree (sp)
end
