# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include <time.h>
include "wcspix.h"


# T_WCSPIX --  Entry point for the WCSPIX Image Support Module for XImtool.
# The WCSPIX task is responsible for converting image coordinates and getting
# pixel values from images of various types.  Results are returned to the
# GUI directly using ISM messaging.

procedure t_wcspix ()

pointer	wp
int	len, disconnect, ncmd
char	socket[SZ_FNAME], cmd[SZ_FNAME], message[SZ_LINE], buf[SZ_DATE]

int	objid, regid
real	x, y
char	ref[SZ_FNAME], template[SZ_LINE], param[SZ_FNAME]

bool	debug

long	clktime()
pointer	wp_init()
int	envgets(), envgeti(), strdic()

# Standard declarations for the Ximtool WCSPIX client interface.
int	xim_connect(), wp_read(), xim_intrhandler()
errchk	wp_read, envgets, envgeti

begin
	call aclrc (message, SZ_LINE)
	call aclrc (cmd, SZ_FNAME)
	call aclrc (socket, SZ_FNAME)

	# Get the connection socket name from the environment if defined
	# or else use the default socket.
        if (envgets ("ISMDEV", socket, SZ_FNAME) <= 0)
	    call strcpy (WCSPIX_CONNECT, socket, SZ_FNAME)

	# Open the socket connection on a negotiated socket.
	if (xim_connect (socket, WCSPIX_NAME, WCSPIX_MODE) == ERR)
	    return

        # Install an interrupt exception handler so we can exit cleanly.
	if (xim_intrhandler() == ERR)
	    return


	# Initialize the task data structures.
	wp = wp_init ()

	# Check for a runtime debug level.
	iferr (WP_DBGLEVEL(wp) = envgeti ("WCSPIX_DEBUG"))
	    WP_DBGLEVEL(wp) = 0

	# Log the connection.
	call wp_cnvdate (clktime(0), buf, SZ_DATE)
	call sprintf (message, SZ_LINE, "info { %s: WCSPIX Connect}\n") 
	    call pargstr (buf)
        call xim_message ("ism_msg", message)

	# Loop over the commands read on the connection and process.
	disconnect = 1
	debug = (WCSPIX_DBG || WP_DBGLEVEL(wp) > 0)
	while (wp_read (message, len) != EOF) {

	    if (debug) {
		call eprintf("message: '%s' len=%d\n")
		    call pargstr (message);  call pargi (len)
	    }
            if (len <= 0) {
		# Server has disconnected.
		disconnect = 0
		break
	    }

            # Scan the command string and get the first word.
            call sscan (message)
            call gargwrd (cmd, SZ_LINE)
            ncmd = strdic (cmd, cmd, SZ_LINE, WCSPIX_CMDS)

            switch (ncmd) {
	    case QUIT:
		# Server wants us to shut down.
		disconnect = 0
		break

	    case INITIALIZE:
		call wp_cnvdate (clktime(0), buf, SZ_DATE)
		call sprintf (message, SZ_LINE,
		    "info { %s: WCSPIX Initialize}\n") 
		        call pargstr (buf)
		call wp_initialize (wp)

	    case CACHE:
		# <ref> <objid> <regid>
		call gargwrd (ref, SZ_FNAME)
		call gargi (objid)
		call gargi (regid)
		if (debug) {
		    call printf ("cache: objid=%d regid=%d ref='%s'\n")
			call pargi(objid); call pargi(regid); call pargstr(ref)
		}

		# Log the event.
		call wp_cnvdate (clktime(0), buf, SZ_DATE)
		call sprintf (message, SZ_LINE,
		    "info { %s: WCSPIX Cache   objid=%3d %s}\n") 
		        call pargstr (buf)
		        call pargi (objid)
		        call pargstr (ref)
        	call xim_message ("ism_msg", message)

		call wp_cache (wp, objid, regid, ref)

	    case UNCACHE:
		# <id>
		call gargi (objid)
		if (debug) { call printf("uncache: id=%d\n");call pargi(objid) }

		# Log the event.
		call wp_cnvdate (clktime(0), buf, SZ_DATE)
		call sprintf (message, SZ_LINE,
		    "info { %s: WCSPIX Uncache objid=%3d}\n") 
		        call pargstr (buf)
		        call pargi (objid)
        	call xim_message ("ism_msg", message)

		call wp_uncache (wp, objid)

	    case WCSTRAN:
		# <id> <x> <y> [[<region> <x> <y>] ["NDC" <x> <y> ]]
		call gargi (objid)
		call gargr (x)  ; call gargr (y)
		if (debug) {
		    call printf ("wcstran: id=%d  (%g,%g)\n")
		        call pargi(objid); call pargr (x); call pargr (y) 
		}
		call wp_wcstran (wp, objid, x, y)

	    case WCSLIST:
		# <id>
		call gargi (objid)
		if (debug) { call printf ("wcslist: id=%d\n");call pargi(objid)}
		call wp_wcslist (wp, objid)

	    case OBJINFO:
		# <id> <template_list>
		call gargi (objid)
		call gargwrd (template, SZ_FNAME)
		if (debug) {
		    call printf ("objinfo: id=%d  temp='%s'\n")
		        call pargi(objid); call pargstr (template); 
		}
		call wp_objinfo (wp, objid, template)

	    case SET:
		# <param> <value>
		call gargwrd (param, SZ_FNAME)
		call wp_setpar (wp, param)

	    case GET:
		# <param>

	    case DEBUG:
		debug = !(debug)

	    default:
		if (debug) { 
	            call eprintf ("ISM default: len=%d msg='%s'\n")
	                call pargi(len); call pargstr(message)
		}
	    }

	    # Clear the buffer for the next read.
	    call aclrc (message, SZ_LINE)
	}

	# Disconnect from the server and clean up.
	call xim_disconnect (disconnect)
	call wp_shutdown (wp)
end


# WP_INITIALIZE -- Initialize the WCSPIX, uncache any previously cached images.

procedure wp_initialize (wp)

pointer	wp					#i WCSPIX structure

pointer	cp, wp_id2obj()
int	i

begin
	for (i=0; i < SZ_CACHE; i=i+1) {
	    cp = wp_id2obj (wp, i)
	    if (cp != NULL && C_OBJID(cp) != NULL)
		call wp_uncache (wp, C_OBJID(cp))
	}
end


# WP_CACHE -- Associate and object reference with a unique object id.

procedure wp_cache (wp, objid, regid, ref)

pointer	wp					#i WCSPIX structure
int	objid					#i object id
int	regid					#i region id
char	ref[ARB]				#i object ref

pointer cp
int	i, class
char	alert[SZ_FNAME]

int	wp_class()

include	"class.com"

begin
	# Find an unused slot in the object cache.
	for (i=0; i < SZ_CACHE; i=i+1) {
	    cp = OBJCACHE(wp,i)
	    if (C_NREF(cp) == 0)
		break
	}

	# Get the object class.
	class = wp_class (ref)
	if (class == ERR) {
	    # Send alert to the GUI.
	    call sprintf (alert, SZ_FNAME, "wp_cache: Unable to cache\n%s")
		call pargstr (ref)
	    call xim_alert (alert, "", "")

	    # Setup for linear system.
	    return
	}
	C_CLASS(cp) = class

	# Initialize the object.
	if (class != NULL && CL_INIT(class) != NULL)
	    call zcall2 (CL_INIT(class), cp, wp)

	# Call the cache function.
	if (class != NULL && CL_CACHE(class) != NULL)
	    call zcall4 (CL_CACHE(class), cp, objid, regid, ref)
end


# WP_UNCACHE -- Remove an object from the WCSPIX cache.

procedure wp_uncache (wp, id)

pointer	wp					#i WCSPIX structure
int	id					#i object id

pointer cp, wp_id2obj()
int	class

include	"class.com"

begin
	cp = wp_id2obj (wp, id)
	if (cp == NULL)
	    return

	# Call the uncache function.
	class = C_CLASS(cp)
	if (class != NULL && CL_UNCACHE(class) != NULL)
	    call zcall2 (CL_UNCACHE(class), cp, id)

	C_NREF(cp) = 0
end


# WP_WCSTRAN -- Translate image coords to WCS values.

procedure wp_wcstran (wp, id, x, y)

pointer	wp					#i WCSPIX structure
int	id					#i object id
real	x, y					#i image coords

pointer	cp, wp_id2obj()
int	class

include	"class.com"

begin
	cp = wp_id2obj (wp, id)
	if (cp == NULL)
	    return

	# Call the uncache function.
	class = C_CLASS(cp)
	if (class != NULL && CL_WCSTRAN(class) != NULL)
	    call zcall4 (CL_WCSTRAN(class), cp, id, x, y)
end


# WP_WCSLIST -- List the available world coordinate systems for the given
# object.

procedure wp_wcslist (wp, id)

pointer	wp					#i WCSPIX structure
int	id					#i object id

pointer	cp, wp_id2obj()
int	class

include	"class.com"

begin
	cp = wp_id2obj (wp, id)
	if (cp == NULL)
	    return

	# Call the uncache function.
	class = C_CLASS(cp)
	if (class != NULL && CL_WCSLIST(class) != NULL)
	    call zcall2 (CL_WCSLIST(class), cp, id)
end


# WP_OBJINFO -- Get and image header or keyword templates for the given
# object.

procedure wp_objinfo (wp, id, template)

pointer	wp					#i WCSPIX structure
int	id					#i object id
char	template[ARB]				#i keyword template

pointer cp, wp_id2obj()
int	class

include	"class.com"

begin
	cp = wp_id2obj (wp, id)
	if (cp == NULL)
	    return

	# Call the uncache function.
	class = C_CLASS(cp)
	if (class != NULL && CL_OBJINFO(class) != NULL)
	    call zcall3 (CL_OBJINFO(class), cp, id, template)
end


# WP_SETPAR -- Set the value of a WCSPIX ISM parameter.

procedure wp_setpar (wp, param)

pointer	wp					#i WCSPIX structure pointer
char	param[SZ_FNAME]				#i WCSPIX param name

char	arg[SZ_PARAM], buf[SZ_PARAM], msg[SZ_PARAM]
int	line

int	strdic()

include	"class.com"

begin
	if (WCSPIX_DBG) { call printf ("set: %s = ");call pargstr(param) }

	switch (strdic (param, param, SZ_PARAM, WCSPIX_PARAMS)) {
	case PAR_PSIZE:
	    call gargi (WP_PTABSZ(wp))
	    if (WCSPIX_DBG) { call printf ("%d\n");call pargi(WP_PTABSZ(wp)) }

	case PAR_BPM:
	    call gargi (WP_BPM(wp))
	    if (WCSPIX_DBG) { call printf ("%d\n");call pargi(WP_BPM(wp)) }

	case PAR_WCS:
	    call gargwrd (buf, SZ_FNAME)
	    call gargi (line)

	    call strcpy (buf, arg, SZ_PARAM)
	    call strlwr (buf)
	    switch (strdic (buf, buf, SZ_FNAME, WCSPIX_SYSTEMS)) {
	    case SYS_LOGICAL:	SYSTEMS(wp,line) = SYS_LOGICAL
	    case SYS_PHYSICAL:	SYSTEMS(wp,line) = SYS_PHYSICAL
	    case SYS_WORLD:	SYSTEMS(wp,line) = SYS_WORLD
	    case SYS_NONE:	SYSTEMS(wp,line) = SYS_NONE
	    case SYS_AMP:	SYSTEMS(wp,line) = SYS_AMP
	    case SYS_CCD:	SYSTEMS(wp,line) = SYS_PHYSICAL
	    case SYS_DETECTOR:	SYSTEMS(wp,line) = SYS_DETECTOR
	    default: 		SYSTEMS(wp,line) = SYS_SKY
	    }
	    call strcpy (buf, WCSNAME(wp,line), LEN_WCSNAME)

	    if (WCSPIX_DBG) { 
		call printf("%s line=%d\n");call pargstr(buf);call pargi(line) }

	    call sprintf (msg, SZ_FNAME, "wcstype %s %d")
		call pargstr (arg)
		call pargi (line)
	    call wcspix_message (msg)

	case PAR_FMT:
	    call gargwrd (buf, SZ_FNAME)
	    call gargi (line)

	    call strcpy (buf, arg, SZ_PARAM)
	    call strlwr (buf)
	    switch (strdic (buf, buf, SZ_FNAME, WCSPIX_FMT)) {
	    case FMT_DEFAULT:  	FORMATS(wp,line) = FMT_DEFAULT
	    case FMT_HMS:  	FORMATS(wp,line) = FMT_HMS
	    case FMT_DEG:  	FORMATS(wp,line) = FMT_DEG
	    case FMT_RAD:  	FORMATS(wp,line) = FMT_RAD
	    default:       	FORMATS(wp,line) = FMT_DEFAULT
	    }

	    if (WCSPIX_DBG) { 
		call printf("%s line=%d\n");call pargstr(buf);call pargi(line) }

	    call sprintf (msg, SZ_FNAME, "wcsfmt %s %d")
		call pargstr (arg)
		call pargi (line)
	    call wcspix_message (msg)
	}
end


# WP_GETPAR -- Get the value of a WCSPIX ISM parameter.

procedure wp_getpar (wp, param)

pointer	wp					#i WCSPIX structure pointer
char	param[SZ_FNAME]				#i WCSPIX param name

int	strdic()

begin
	if (WCSPIX_DBG) { call printf ("set: %s = ");call pargstr(param) }

	switch (strdic (param, param, SZ_PARAM, WCSPIX_PARAMS)) {
	case PAR_PSIZE:
	case PAR_BPM:
	case PAR_WCS:
	case PAR_FMT:
	}
end


################################################################################
#
# Private procedures.
#
################################################################################


# WP_INIT -- Initialize the WCSPIX task and data structures.

pointer procedure wp_init ()

pointer	wp					#r WCSPIX structure pointer
int	i

begin
	# Allocate the task structure.
        iferr (call calloc (wp, SZ_WCSPIX, TY_STRUCT))
            call error (0, "Error opening WCSPIX task structure.")

	call calloc (WP_SYSTEMS(wp), MAX_WCSLINES, TY_INT)
	call calloc (WP_FORMATS(wp), MAX_WCSLINES, TY_INT)
	call calloc (WP_WCS(wp), (LEN_WCSNAME*MAX_WCSLINES), TY_CHAR)
	for (i=1; i <= MAX_WCSLINES; i=i+1) {
	    FORMATS(wp,i) = DEF_FMT
	    SYSTEMS(wp,i) = DEF_SYSTEM
	    call strcpy ("none", WCSNAME(wp,i), LEN_WCSNAME)
	}

	# Allocate the object cache.
	call calloc (WP_CPTR(wp), SZ_CACHE, TY_STRUCT)
	for (i=0; i < SZ_CACHE; i=i+1)
	    call calloc (OBJCACHE(wp,i), SZ_CNODE, TY_STRUCT)

	WP_PTABSZ(wp) = DEF_PTABSZ
	WP_BPM(wp)    = DEF_BPM_FLAG

	# Initialize the class modules.
	call wp_class_init()

	return (wp)
end


# WP_READ -- Read messages from the connection and process them optimally for
# this ISM.  This means we segment the messages and handle only the last
# few WCS requests so we can keep up with the server requests.  Presumably
# there are more cursor events coming which are no longer valid so some are
# thrown out.

int procedure wp_read (message, len)

char	message[ARB]				#o message buffer
int	len					#o length of message

int	nread

int	xim_read()				# low-level i/o
errchk	xim_read

begin
	nread = xim_read (message, len)

	return (nread)
end


# WP_SHUTDOWN -- Shut down the WCSPIX, freeing all storage

procedure wp_shutdown (wp)

pointer	wp					#i WCSPIX structure
int	i

begin
	# Free the structures.
	call mfree (WP_WCS(wp), TY_CHAR)
	call mfree (WP_FORMATS(wp), TY_INT)
	call mfree (WP_SYSTEMS(wp), TY_INT)
	for (i=0; i < SZ_CACHE; i=i+1)
	    call mfree (OBJCACHE(wp,i), TY_STRUCT)

	call mfree (WP_CPTR(wp), TY_STRUCT)
	call mfree (wp, TY_STRUCT)
end


# WP_CLASS -- Determine the object class for the named image/file.

int procedure wp_class (object)

char	object[ARB]				#i object reference

int	n, class
pointer	im
char	ch, buf[SZ_FNAME]

int	strlen(), stridx()
bool	streq()
pointer	immap()

errchk	immap

begin
        # The following kludge is necessary to protect against the case
        # where dev$pix is used as a test image.  The 'object' pathname in
        # this case is "node!/path/dev/pix" which lacks the extension
        # and causes the task to fail to open because of a conflict with
        # the pix.hhh in the same directory.  Most IRAF tasks work since
        # the imio$iki code treats the string "dev$pix" as a special case.

	call imgimage (object, buf, SZ_FNAME)
        n = strlen (buf) - 7
        if (streq (buf[n], "/dev/pix")) {
            call strcpy ("dev$pix", buf, SZ_FNAME)
	    ch = '['
	    n = stridx (ch, object)
	    if (n > 0)
                call strcat (object[n], buf, SZ_FNAME)
            call strcpy (buf, object, SZ_FNAME)
	}


	# See if we can map the image to get at least an image class.  If
	# so then check for special subclasses like Mosaic files, spectra, etc.

	class =  UNKNOWN_CLASS
	ifnoerr (im = immap (object, READ_ONLY, 0)) {
	    class = IMAGE_CLASS

	    # Now check for subclasses.  (TBD)

	    call imunmap (im)
	}

	return (class)
end


# WP_ID2OBJ -- Utility routine to convert and object id to the cache pointer.

pointer procedure wp_id2obj (wp, id)

pointer	wp					#i WCSPIX structure
int	id					#i object id

int	i
pointer	cp

begin
	for (i=0; i < SZ_CACHE; i=i+1) {
	    cp = OBJCACHE(wp,i)
	    if (C_OBJID(cp) == id)
		return (cp)
	}
	return (NULL)
end


# WP_CLASS_INIT -- Initialize the WCSPIX ISM class modules.

procedure wp_class_init()

extern  img_init(), img_cache(), img_uncache()
extern  img_wcstran(), img_wcslist(), img_objinfo()

extern  mef_init(), mef_cache(), mef_uncache()
extern  mef_wcstran(), mef_wcslist(), mef_objinfo()

extern  msp_init(), msp_cache(), msp_uncache()
extern  msp_wcstran(), msp_wcslist(), msp_objinfo()

extern  unk_init(), unk_cache(), unk_uncache()
extern  unk_wcstran(), unk_wcslist(), unk_objinfo()

include	"class.com"
int     locpr()

begin
	cl_nclass = 0

	# Load the class modules.
	call wp_load_class ("unknown",
	    locpr(unk_init), locpr(unk_cache), locpr(unk_uncache),
	    locpr(unk_wcstran), locpr(unk_wcslist), locpr(unk_objinfo))
	call wp_load_class ("image",
	    locpr(img_init), locpr(img_cache), locpr(img_uncache),
	    locpr(img_wcstran), locpr(img_wcslist), locpr(img_objinfo))
	call wp_load_class ("mef",
	    locpr(mef_init), locpr(mef_cache), locpr(mef_uncache),
	    locpr(mef_wcstran), locpr(mef_wcslist), locpr(mef_objinfo))
	call wp_load_class ("multispec",
	    locpr(msp_init), locpr(msp_cache), locpr(msp_uncache),
	    locpr(msp_wcstran), locpr(msp_wcslist), locpr(msp_objinfo))
end


# WP_LOAD_CLASS -- Load an object class module for the ISM task.

procedure wp_load_class (name, init, cache, uncache, tran, list, info)

char	name[ARB]			#I module name
int	init				#I initialize procedure
int	cache				#I cache the object procedure
int	uncache				#I uncache the object procedure
int	tran				#I translate WCS procedure
int	list				#I list WCS proedure
int	info				#I get header procedure

errchk	syserrs
include	"class.com"

begin
	# Get a new driver slot.
	if (cl_nclass + 1 > MAX_CLASSES)
	    return
	cl_nclass = cl_nclass + 1

	# Load the driver.
	CL_INIT(cl_nclass) = init
	CL_CACHE(cl_nclass) = cache
	CL_UNCACHE(cl_nclass) = uncache
	CL_WCSTRAN(cl_nclass) = tran
	CL_WCSLIST(cl_nclass) = list
	CL_OBJINFO(cl_nclass) = info
	call strcpy (name, CL_NAME(cl_nclass), SZ_FNAME)
end


# WCSPIX_MESSAGE -- Deliver a message to the ISM callback, tagged with
# our name so it can be passed off to the correct code.

procedure wcspix_message (message)

char    message[ARB]                            #I message to send

pointer sp, msgbuf
int     msglen, mlen, ip 

int	strlen()

begin
        # Get the message length plus some extra for the braces and padding.
        mlen = strlen (message)
        msglen = mlen + 64

        # Allocate and clear the message buffer.
        call smark (sp)
        call salloc (msgbuf, msglen, TY_CHAR)
        call aclrc (Memc[msgbuf], msglen)

	ip = 0
	call amovc ("deliver wcspix { ", Memc[msgbuf], 17)  ; ip = ip + 17
	call amovc (message, Memc[msgbuf+ip], mlen)	    ; ip = ip + mlen
	call amovc (" }\0", Memc[msgbuf+ip], 2)		    ; ip = ip + 2

	call xim_message ("ism_msg", Memc[msgbuf])

	call sfree (sp)
end


define	SZ_WEEKDAY		3
define	SZ_MONTH		3

# WP_CNVDATE -- Convert a time in integer seconds since midnight on Jan 1, 1980
# into a short string such as "5/15 18:24". 

procedure wp_cnvdate (ltime, outstr, maxch)

long	ltime			# seconds since 00:00:00 10-Jan-1980
char	outstr[ARB]
int	maxch

int	tm[LEN_TMSTRUCT]

begin
	call brktime (ltime, tm)

# 	call sprintf (outstr, maxch, "%2d/%2d %2d:%02d")
# 	    call pargi (TM_MONTH(tm))
# 	    call pargi (TM_MDAY(tm))
# 	    call pargi (TM_HOUR(tm))
# 	    call pargi (TM_MIN(tm))

#	call sprintf (outstr, maxch, "%2d:%02d")
#	    call pargi (TM_HOUR(tm))
#	    call pargi (TM_MIN(tm))

	call sprintf (outstr, maxch, "%2d:%02d:%02d")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
end



#----------------
# DEBUG ROUTINES.
#----------------
procedure dbg_printcache (wp, buf)
pointer	wp
char	buf[ARB]
pointer	cp, wp_id2obj()
int	i
begin
	call printf ("%s\n") ; call pargstr (buf)
	for (i=0; i < SZ_CACHE; i=i+1) {
	    cp = wp_id2obj (wp, i)
	    if (C_DATA(cp) != NULL) {
		call printf ("%3d:  id=%d  ref='%s'\n")
		    call pargi(i)
		    call pargi(C_OBJID(cp))
		    call pargstr(C_REF(cp))
	    }
	}
end
