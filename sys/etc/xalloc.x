# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<xalloc.h>
include	<syserr.h>
include	<ctype.h>
include	<knet.h>

.helpsys xalloc
.nf _________________________________________________________________________
XALLOC -- Device allocation package.

	  xallocate (device)
	xdeallocate (device, rewind)
	  xdevowner (device, owner, maxch)
	 xdevstatus (device, out)
	  xgdevlist (device, devlist, maxch, onedev)

status:

	DV_DEVFREE	device is free and can be allocated
	DV_DEVALLOC	device is already allocated
	DV_DEVINUSE	device is in use by someone else
	DV_DEVNOTFOUND	device is not in device table

The allocatable devices are defined in the text file dev$tapecap.
.endhelp ____________________________________________________________________

define	SZ_DEVLIST	256
define	ALLOCATE	1
define	DEALLOCATE	0


# XALLOCATE -- Attempt to allocate the named device, i.e., allocate the device
# for exclusive i/o, and ready it for i/o following some sort of OPEN call.
# Allocate performs the function called "mount" on some systems, as well as
# allocating the device.

int procedure xallocate (device)

char	device[ARB]		#I device to be allocated

pointer	sp, devlist
int	status, onedev
int	xgdevlist(), mtfile()
errchk	xgdevlist, mtallocate
define	done_ 91

begin
	call smark (sp)
	call salloc (devlist, SZ_DEVLIST, TY_CHAR)

	# Fetch the device list for the named device.
	onedev = NO
	status = xgdevlist (device, Memc[devlist], SZ_DEVLIST, onedev)
	if (status != OK)
	    goto done_

	# Attempt to allocate the device at the host system level.
	call strpak (Memc[devlist], Memc[devlist], SZ_DEVLIST)
	call zdvall (Memc[devlist], ALLOCATE, status)

	# If that worked and the device is a magtape, call MTIO to complete
	# the allocation process.

	if (status == OK && mtfile (device) == YES)
	    call mtallocate (device)
done_
	call sfree (sp)
	return (status)
end


# XDEALLOCATE -- Deallocate the named device.

int procedure xdeallocate (device, rewind)

char	device[ARB]		#I device to be deallocated
int	rewind			#I rewind if magtape?

int	status, onedev
pointer	sp, devlist, osdev, owner
int	xgdevlist(), mtfile()
errchk	xgdevlist, syserrs
define	done_ 91

begin
	call smark (sp)
	call salloc (devlist, SZ_DEVLIST, TY_CHAR)
	call salloc (osdev, SZ_FNAME, TY_CHAR)
	call salloc (owner, SZ_FNAME, TY_CHAR)

	# Get the i/o device name.
	onedev = YES
	status = xgdevlist (device, Memc[osdev], SZ_FNAME, onedev)
	if (status != OK)
	    goto done_

	# Verify that the device is actually allocated.  If the device is a
	# magtape, call MTIO to conditionally rewind the drive and deallocate
	# the drive in MTIO.

	call strpak (Memc[osdev], Memc[osdev], SZ_FNAME)
	call zdvown (Memc[osdev], Memc[owner], SZ_FNAME, status)
	if (status != DV_DEVALLOC)
	    call syserrs (SYS_MTNOTALLOC, device)
	else if (mtfile (device) == YES)
	    call mtdeallocate (device, rewind)

	# Fetch the device list for the named device.
	onedev = NO
	status = xgdevlist (device, Memc[devlist], SZ_DEVLIST, onedev)
	if (status != OK)
	    goto done_

	# Physically deallocate the device.
	call strpak (Memc[devlist], Memc[devlist], SZ_DEVLIST)
	call zdvall (Memc[devlist], DEALLOCATE, status)
done_
	call sfree (sp)
	return (status)
end


# XDEVSTATUS -- Print the status of the named device on the output file.

procedure xdevstatus (device, out)

char	device[ARB]		#I device
int	out			#I output file

int	status
char	owner[SZ_FNAME]
int	xdevowner(), mtfile()
errchk	xdevowner, mtfile

begin
	status = xdevowner (device, owner, SZ_FNAME)

	switch (status) {
	case DV_DEVFREE:
	    call fprintf (out, "device %s is not currently allocated\n")
		call pargstr (device)
	    if (mtfile (device) == YES)
		iferr (call mtstatus (out, device))
		    ;
	case DV_DEVINUSE:
	    call fprintf (out, "device %s is currently allocated to %s\n")
		call pargstr (device)
		call pargstr (owner)
	case DV_DEVALLOC:
	    if (mtfile (device) == YES)
		call mtstatus (out, device)
	    else {
		call fprintf (out, "device %s is allocated\n")
		    call pargstr (device)
	    }
	default:
	    call fprintf (out, "cannot get device status for `%s'\n")
		call pargstr (device)
	}
end


# XDEVOWNER -- Determine whether or not the named device is already
# allocated, and if the device is currently allocated to someone else,
# return the owner name.

int procedure xdevowner (device, owner, maxch)

char	device[ARB]		#I device to be deallocated
char	owner[maxch]		#O receives owner name
int	maxch			#I max chars out

pointer	sp, devlist
int	status, onedev
int	xgdevlist()
errchk	xgdevlist
define	done_ 91

begin
	call smark (sp)
	call salloc (devlist, SZ_DEVLIST, TY_CHAR)

	# Fetch the device list for the named device.
	onedev = YES
	status = xgdevlist (device, Memc[devlist], SZ_DEVLIST, onedev)
	if (status != OK)
	    goto done_

	# Query device allocation.
	call strpak (Memc[devlist], Memc[devlist], SZ_DEVLIST)
	call zdvown (Memc[devlist], owner, maxch, status)
	call strupk (owner, owner, maxch)
done_
	call sfree (sp)
	return (status)
end
