# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xalloc.h>
include	<knet.h>

# MT_DEVALLOCATED -- Verify that the named host magtape device is allocated.
# The IRAF device name must already have been translated to a host device
# name before we are called.

int procedure mt_devallocated (osdev)

char	osdev[ARB]		# host name of device
int	status
pointer	sp, pk_osdev, pk_owner

begin
	call smark (sp)
	call salloc (pk_osdev, SZ_FNAME, TY_CHAR)
	call salloc (pk_owner, SZ_FNAME, TY_CHAR)

	call strpak (osdev, Memc[pk_osdev], SZ_FNAME)
	call zdvown (Memc[pk_osdev], Memc[pk_owner], SZ_FNAME, status)

	call sfree (sp)
	if (status == DV_DEVALLOC)
	    return (YES)
	else
	    return (NO)
end
