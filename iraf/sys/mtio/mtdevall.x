# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xalloc.h>
include	<knet.h>

# MT_DEVALLOCATED -- Verify that the named host magtape device is allocated.
# The IRAF device name must already have been translated to a host device
# name before we are called.

int procedure mt_devallocated (iodev)

char	iodev[ARB]			#I host name of device
pointer	sp, pk_iodev, pk_owner
int	status

begin
	call smark (sp)
	call salloc (pk_iodev, SZ_FNAME, TY_CHAR)
	call salloc (pk_owner, SZ_FNAME, TY_CHAR)

	# The following assumes that the node! prefix is in iodev.
	call strpak (iodev, Memc[pk_iodev], SZ_FNAME)
	call zdvown (Memc[pk_iodev], Memc[pk_owner], SZ_FNAME, status)

	call sfree (sp)
	if (status == DV_DEVALLOC)
	    return (YES)
	else
	    return (NO)
end
