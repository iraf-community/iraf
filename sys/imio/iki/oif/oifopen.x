# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<fio.h>
include	"oif.h"

# OIF_OPEN -- Open/create an image.

procedure oif_open (kernel, im, o_im, root, extn, ksection, cl_index, cl_size, acmode, status)

int	kernel			#I IKI kernel
pointer	im			#I image descriptor
pointer	o_im			#I old image, if new_copy image
char	root[ARB]		#I root image name
char	extn[ARB]		#I extension, if any
char	ksection[ARB]		#I NOT USED
int	cl_index		#I NOT USED
int	cl_size			#I NOT USED
int	acmode			#I access mode
int	status			#O return value

pointer	sp, fname, pixfile
int	hfd, nchars, mode, junk
int	open(), oif_rdhdr(), access(), protect(), envgeti()
bool	envgetb(), fnullfile()
errchk	syserrs
define	err_ 91

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (pixfile, SZ_PATHNAME, TY_CHAR)

	status = OK

	# The only valid cl_index is -1 (none specified) or 1.
	if (!(cl_index < 0 || cl_index == 1))
	    goto err_

	# This kernel does not permit a kernel section to be used.
	if (ksection[1] != EOS)
	    call syserrs (SYS_IKIKSECTNS, Memc[fname])

	# Determine access mode for header file.
	if (acmode == NEW_COPY || acmode == NEW_IMAGE)
	    mode = NEW_FILE
	else
	    mode = acmode

	# Generate full header file name; the extension may be either ".imh"
	# or nothing, and was set earlier by oif_access().

	if (mode == NEW_FILE && extn[1] == EOS)
	    call iki_mkfname (root, OIF_HDREXTN, Memc[fname], SZ_PATHNAME)
	else
	    call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)

	# Delete any old image if one exists and imclobber is enabled.
	if (mode == NEW_FILE && !fnullfile (Memc[fname]) &&
	    (access (Memc[fname], 0,0) == YES)) {

	    if (envgetb ("imclobber")) {
		iferr (hfd = open (Memc[fname], READ_ONLY, BINARY_FILE)) {
		    status = ERR
		    goto err_
		}
		nchars = LEN_IMHDR * SZ_MII_INT
		if (oif_rdhdr (hfd, im, nchars, TY_IMHDR) < 0) {
		    status = ERR
		    goto err_
		}
		if (IM_PIXFILE(im) != EOS) {
		    call oif_gpixfname (IM_PIXFILE(im), IM_HDRFILE(im),
			Memc[pixfile], SZ_PATHNAME)
		    if (access (Memc[pixfile],0,0) == YES)
			iferr (call delete (Memc[pixfile]))
			    call erract (EA_WARN)
		}
		call close (hfd)
		iferr (junk = protect (Memc[fname], REMOVE_PROTECTION))
		    ;
		iferr (call delete (Memc[fname]))
		    call erract (EA_WARN)
	    } else
		call syserrs (SYS_IKICLOB, Memc[fname])
	}

	# Open the image header file.
	iferr (hfd = open (Memc[fname], mode, BINARY_FILE))
	    goto err_

	IM_HFD(im) = hfd

	# If opening an existing image, read the OIF fixed format binary
	# image header into the image descriptor.  If opening a new image,
	# write out a generic image header so that the image can be accessed
	# and deleted with imdelete should the operation be aborted before
	# a full image is written.

	if (mode == NEW_FILE) {
	    iferr (IM_HDRVER(im) = envgeti (ENV_OIFVER))
		IM_HDRVER(im) = DEF_VERSION
	    call aclrc (IM_HDRFILE(im), SZ_IMHDRFILE)
	    call strcpy (Memc[fname], IM_HDRFILE(im), SZ_IMHDRFILE)
	    iferr (call oif_updhdr (im, status))
		;
	} else {
	    iferr {
		nchars = (IM_LENHDRMEM(im) - LEN_IMHDR) * SZ_MII_INT
		if (oif_rdhdr (hfd, im, nchars, TY_IMHDR) < 0)
		    status = ERR
		else {
		    call aclrc (IM_HDRFILE(im), SZ_IMHDRFILE)
		    call strcpy (Memc[fname], IM_HDRFILE(im), SZ_IMHDRFILE)
		}
	    } then
		status = ERR
	}

	# It is best to close the header file at this point for two reasons:
	# to improve error recovery (if an abort occurs with a new file still
	# open FIO will delete it) and to free file descriptors (important for
	# applications that open many images).  If the header needs to be
	# updated, oif_updhdr will reopen the header file.

	call close (hfd)
	IM_HFD(im) = NULL

	call sfree (sp)
	return
err_
	status = ERR
	call sfree (sp)
end
