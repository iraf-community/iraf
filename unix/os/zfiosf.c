/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/*
 * ZFIOSF -- Static file device driver.  In the 4.1BSD UNIX kernel the ordinary
 * binary file driver is used for static files (files that do not change in
 * size once created by ZFALOC), hence all we need do to implement a SF routine
 * is call the corresponding BF routine.  A great gain in i/o efficiency could 
 * probably be gained by replacing this driver by one using raw i/o, but we do
 * not want to bother with that for 4.1BSD UNIX because the time would be
 * better spent doing it for 4.2BSD.
 *
 * If anyone is stuck with 4.1BSD for some reason and wants fast static file
 * i/o, the strategy is to try to allocate contiguous files with ZFALOC, either
 * in a raw partition using a special file manager or within an ordinary
 * partition adding a new system call to allocate contiguous storage for a file.
 * The latter scheme has the advantage that files thus created are ordinary
 * UNIX files and can be accessed normally as well as by the static file driver.
 * Given a contiguous or near-contiguous file on disk, all the static file
 * driver needs for direct access with large transfers is the physical block
 * offset of the file.  The raw device is then accessed via physio calls to
 * transfer data directly to or from the user's buffer, bypassing the system
 * buffer cache.  Write perm is required on the raw device for the target
 * filesystem; this opens up the possibility of trashing the files system.
 * Static file access should be restricted to one or more large temporary files
 * systems.  If one gets really ambitious a special UNIX driver can be added to
 * permit asynchronous i/o, bypassing the UNIX files system entirely except
 * during file creation and deletion.
 */


/* ZOPNSF -- Open a static file.  Only RO, WO, and RW modes are permitted
 * for static files, since allocation is via ZFALOC and appending is not
 * permitted.
 */
ZOPNSF (osfn, mode, chan)
PKCHAR	*osfn;			/* UNIX name of file		*/
XINT	*mode;			/* file access mode		*/
XINT	*chan;			/* file number (output)		*/
{
	switch (*mode) {
	case READ_ONLY:
	case WRITE_ONLY:
	case READ_WRITE:
	    ZOPNBF (osfn, mode, chan);
	    break;
	default:
	    *chan = XERR;
	}
}


/* ZCLSSF -- Close a static file.
 */
ZCLSSF (fd, status)
XINT	*fd;
XINT	*status;
{
	ZCLSBF (fd, status);
}


/* ZARDSF -- "Asynchronous" static block read.  Initiate a read of at most
 * maxbytes bytes from the file FD into the buffer BUF.  Status is returned
 * in a subsequent call to ZAWTSF.
 */
ZARDSF (chan, buf, maxbytes, offset)
XINT	*chan;			/* UNIX file number			*/
XCHAR	*buf;			/* output buffer			*/
XINT	*maxbytes;		/* max bytes to read			*/
XLONG	*offset;		/* 1-indexed file offset to read at	*/
{
	ZARDBF (chan, buf, maxbytes, offset);
}


/* ZAWRSF -- "Asynchronous" static block write.  Initiate a write of exactly
 * nbytes bytes from the buffer BUF to the file FD.  Status is returned in a
 * subsequent call to ZAWTSF.
 */
ZAWRSF (chan, buf, nbytes, offset)
XINT	*chan;			/* UNIX file number		*/
XCHAR	*buf;			/* buffer containing data	*/
XINT	*nbytes;		/* nbytes to be written		*/
XLONG	*offset;		/* 1-indexed file offset	*/
{
	ZAWRBF (chan, buf, nbytes, offset);
}


/* ZAWTSF -- "Wait" for an "asynchronous" read or write to complete, and
 * return the number of bytes read or written, or ERR.
 */
ZAWTSF (fd, status)
XINT	*fd;
XINT	*status;
{
	ZAWTBF (fd, status);
}


/* ZSTTSF -- Return status on a static file.
 */
ZSTTSF (fd, param, lvalue)
XINT	*fd;
XINT	*param;
XLONG	*lvalue;
{
	ZSTTBF (fd, param, lvalue);
}
