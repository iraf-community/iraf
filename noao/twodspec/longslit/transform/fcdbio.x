include	<error.h>
include	<math/gsurfit.h>
include	<pkg/dttext.h>

# FC_DBWRITE -- Write an fitcoords database entry.

procedure fc_dbwrite (database, fitname, axis, sf)

char	database[ARB]		# Database
char	fitname[ARB]		# Database fit name
int	axis			# Axis for surface
pointer	sf			# Surface pointer

int	i, nsave
pointer	dt, coeffs, sp, dbfile

int	xgsgeti()
pointer	dtmap1()

begin
	if (sf == NULL)
	    return

	call smark (sp)
	call salloc (dbfile, SZ_FNAME, TY_CHAR)
	call sprintf (Memc[dbfile], SZ_FNAME, "fc%s")
	    call pargstr (fitname)
	dt = dtmap1 (database, Memc[dbfile], APPEND)

	call dtptime (dt)
	call dtput (dt, "begin\t%s\n")
	    call pargstr (fitname)
	call dtput (dt, "\ttask\tfitcoords\n")
	call dtput (dt, "\taxis\t%d\n")
	    call pargi (axis)

	nsave = xgsgeti (sf, GSNSAVE)
	call salloc (coeffs, nsave, TY_DOUBLE)
	call xgssave (sf, Memd[coeffs])
	call dtput (dt, "\tsurface\t%d\n")
	    call pargi (nsave)
	do i = 1, nsave {
	    call dtput (dt, "\t\t%g\n")
		call pargd (Memd[coeffs+i-1])
	}

	call sfree (sp)
	call dtunmap (dt)
end


# LM_DBREAD -- Read an lsmap database entry.

procedure lm_dbread (database, fitname, axis, sf)

char	database[ARB]		# Database
char	fitname[ARB]		# Fit name
int	axis			# Axis for surface
pointer	sf			# Surface pointer

int	rec, ncoeffs
pointer	dt, coeffs, sp, dbfile

int	dtlocate(), dtgeti()
pointer	dtmap1()

errchk	dtlocate(), dtgeti(), dtgad()

begin
	sf = NULL
	coeffs = NULL

	call smark (sp)
	call salloc (dbfile, SZ_FNAME, TY_CHAR)
	call sprintf (Memc[dbfile], SZ_FNAME, "fc%s")
	    call pargstr (fitname)
	dt = dtmap1 (database, Memc[dbfile], READ_ONLY)

	rec = dtlocate (dt, fitname)
	axis = dtgeti (dt, rec, "axis")
	ncoeffs = dtgeti (dt, rec, "surface")
	call salloc (coeffs, ncoeffs, TY_DOUBLE)
	call dtgad (dt, rec, "surface", Memd[coeffs], ncoeffs, ncoeffs)
	call xgsrestore (sf, Memd[coeffs])

	call sfree (sp)
	call dtunmap (dt)
end
