include	<error.h>
include	<math/gsurfit.h>
include	<pkg/dttext.h>
include	<units.h>

# FC_DBWRITE -- Write an fitcoords database entry.

procedure fc_dbwrite (database, fitname, axis, un, sf)

char	database[ARB]		# Database
char	fitname[ARB]		# Database fit name
int	axis			# Axis for surface
pointer	un			# Units pointer
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
	call strcpy ("fc", Memc[dbfile], SZ_FNAME)
	call imgcluster (fitname, Memc[dbfile+2], SZ_FNAME-2)
	dt = dtmap1 (database, Memc[dbfile], APPEND)

	call dtptime (dt)
	call dtput (dt, "begin\t%s\n")
	    call pargstr (fitname)
	call dtput (dt, "\ttask\tfitcoords\n")
	call dtput (dt, "\taxis\t%d\n")
	    call pargi (axis)
	if (un != NULL) {
	    call dtput (dt, "\tunits\t%s\n")
		call pargstr (UN_UNITS(un))
	}

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

procedure lm_dbread (database, fitname, axis, un, sf)

char	database[ARB]		# Database
char	fitname[ARB]		# Fit name
int	axis			# Axis for surface
pointer	un			# Units pointer
pointer	sf			# Surface pointer

int	rec, ncoeffs
pointer	dt, coeffs, sp, dbfile, units

int	dtlocate(), dtgeti()
pointer	dtmap1(), un_open()

errchk	dtlocate(), dtgeti(), dtgad(), un_open()

begin
	un = NULL
	sf = NULL
	coeffs = NULL

	call smark (sp)
	call salloc (dbfile, SZ_FNAME, TY_CHAR)
	call salloc (units, SZ_FNAME, TY_CHAR)
	call strcpy ("fc", Memc[dbfile], SZ_FNAME)
	call imgcluster (fitname, Memc[dbfile+2], SZ_FNAME-2)
	dt = dtmap1 (database, Memc[dbfile], READ_ONLY)

	rec = dtlocate (dt, fitname)
	axis = dtgeti (dt, rec, "axis")
	ifnoerr (call dtgstr (dt, rec, "units", Memc[units], SZ_FNAME))
	    un = un_open (Memc[units])
	ncoeffs = dtgeti (dt, rec, "surface")
	call salloc (coeffs, ncoeffs, TY_DOUBLE)
	call dtgad (dt, rec, "surface", Memd[coeffs], ncoeffs, ncoeffs)
	call xgsrestore (sf, Memd[coeffs])

	call sfree (sp)
	call dtunmap (dt)
end
