# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xalloc.h>
include	<knet.h>

# MT_DEVALLOCATED -- Verify that the named host magtape device is allocated.
# The IRAF device name must already have been translated to a host device
# name before we are called.

int procedure mt_devallocated (iodev)

char	iodev[ARB]			#I host name of device

pointer	sp, pk_iodev, pk_owner
size_t	sz_val
int	status

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (pk_iodev, sz_val, TY_CHAR)
	call salloc (pk_owner, sz_val, TY_CHAR)

	# The following assumes that the node! prefix is in iodev.
	sz_val = SZ_FNAME
	call strpak (iodev, Memc[pk_iodev], sz_val)
	call zdvown (Memc[pk_iodev], Memc[pk_owner], SZ_FNAME, status)

	call sfree (sp)
	if (status == DV_DEVALLOC)
	    return (YES)
	else
	    return (NO)
end
