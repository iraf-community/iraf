# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<knet.h>

# FCHDIR -- Change the current working directory.

procedure fchdir (newdir)

char	newdir[ARB]

int	ip, status
pointer	sp, vfn, osfn1, osfn2
int	ki_extnode(), envfind()
errchk	fmapfn, ki_extnode

begin
	call smark (sp)
	call salloc (vfn,   SZ_FNAME,    TY_CHAR)
	call salloc (osfn1, SZ_PATHNAME, TY_CHAR)
	call salloc (osfn2, SZ_PATHNAME, TY_CHAR)

	call strcpy (newdir, Memc[vfn], SZ_FNAME)

	# Check for names of the form "node!" and convert them into
	# "node!home$".  This will also convert the null string into
	# a chdir to home$.

	ip = ki_extnode (Memc[vfn], Memc[osfn1], SZ_PATHNAME, status)
	if (newdir[ip+1] == EOS)
	    call strcat ("home$", Memc[vfn], SZ_FNAME)

	# Try the name as is.
	call fmapfn (Memc[vfn], Memc[osfn1], SZ_PATHNAME)
	call strupk (Memc[osfn1], Memc[osfn1], SZ_PATHNAME)

	call zfpath (Memc[osfn1], Memc[osfn2], SZ_PATHNAME, status)
	call zfsubd (Memc[osfn2], SZ_PATHNAME, "", status)

	call strpak (Memc[osfn2], Memc[osfn2], SZ_PATHNAME)
	call zfchdr (Memc[osfn2], status)

	# Try chdir ldir$.
	if (status == ERR) {
	    call strcpy (Memc[vfn], Memc[osfn1], SZ_FNAME)
	    if (envfind (Memc[osfn1], Memc[osfn2], SZ_PATHNAME) > 0) {
		call strcat ("$", Memc[osfn1], SZ_PATHNAME)
		call fmapfn (Memc[osfn1], Memc[osfn2], SZ_PATHNAME)
		call zfchdr (Memc[osfn2], status)
	    } else
		status = ERR
	}

	call sfree (sp)
	if (status == ERR)
	    call syserrs (SYS_FCHDIR, newdir)
end
