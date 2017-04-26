# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <config.h>
include <mach.h>
include <xwhen.h>


# ISM.X -- Interface routines for client programs to connect to the
# XImtool ISM port on the local socket
#
#	  status = ism_connect  (device, name, mode)
#		ism_disconnect  (send_quit)
#	           ism_message  (object, message)
#	             ism_alert  (text, ok_action, cancel_action)
#
#	             ism_write  (message, len)
#	     nread =  ism_read  (message, len)
#
# Client programs should install an exception handler to first disconnect
# from the device before shutting down.  The procedure ism_zxwhen() is
# provided for this purpose.


define	ISM_DBG		FALSE

define	SZ_MESSAGE	2047

define	ISM_TEXT	1
define	ISM_BINARY	2


# ISM_CONNECT -- Negotiate a connection on the named device.  Once
# established we can begin sending and reading messages from the server.

int procedure ism_connect (device, name, type)

char	device[ARB]				#I socket to connect on
char	name[ARB]				#I module name
char	type[ARB]				#I requested connection mode

pointer	sp, cmsg, dev, buf
int	msglen
char	connect[SZ_FNAME]

int	ndopen(), reopen(), strlen()
int	ism_read()
bool 	streq()

extern	ism_onerror()

include	"ismfd.com"				# I/O common
include "ismcom.com" 				# Interrupt handler variables

# Exception handler variables common.
int     ism_errstat
data  	ism_errstat /OK/
common  /ismecom/ ism_errstat

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (cmsg, SZ_LINE, TY_CHAR)
	call salloc (dev, SZ_FNAME, TY_CHAR)

	# Initialize.
	call aclrc (Memc[buf], SZ_LINE)
	call aclrc (Memc[cmsg], SZ_LINE)
	call aclrc (Memc[dev], SZ_FNAME)
	call aclrc (buffer, 2*SZ_MESSAGE+1)
	fdin  = NULL
	fdout = NULL
	nbuf  = 0
	bp  = 0
	ep  = 0

	# Generate the device name.  We assume the call was made with either
	# a "unix:" or "inet:" prefix, so just append the type and set the
	# mode.

	call sprintf (Memc[dev], SZ_FNAME, "%s:%s")
	    call pargstr (device)
	    call pargstr (type)
	if (streq (type, "text"))
	    mode = ISM_TEXT
	else
	    mode = ISM_BINARY

	# Open the initial connection
        iferr (fdin  = ndopen (Memc[dev], READ_WRITE)) {
	    call sfree (sp)
            return (ERR)
	}
	fdout = reopen (fdin, READ_WRITE)

	# Send the connect request.
	call sprintf (Memc[cmsg], SZ_LINE, "connect %s\0")
	    call pargstr (name)
	msglen = strlen (Memc[cmsg])
	call ism_message ("ximtool", Memc[cmsg])

	# Read the acknowledgement.
	if (ism_read (Memc[buf], msglen) == EOF) {
	    call sfree (sp)
	    return (ERR)
	}

	# Close the original socket.
	call close (fdout)
	call close (fdin)

	# Get the new device name.
	call sprintf (connect, SZ_LINE, "unix:%s:%s\0")
	    call pargstr (Memc[buf+8])
	    call pargstr (type)

	# Open the new channel.
        iferr (fdin = ndopen (connect, READ_WRITE)) {
	    call sfree (sp)
            return (ERR)
	}
	fdout = reopen (fdin, READ_WRITE)

	if (ISM_DBG) {
	    call eprintf ("Reconnected on '%s'\n"); call pargstr (connect)
	}

	# Tell the server we're ready to begin.
	call sprintf (Memc[cmsg], SZ_LINE, "ready %s\0")
	    call pargstr (name)
	msglen = strlen (Memc[cmsg])
	call ism_message ("ximtool", Memc[cmsg])


        # Post the ism_onerror procedure to be executed upon process shutdown
        # to issue a warning to the server in case we don't close normally.

        call onerror (ism_onerror)

	call sfree (sp)
	return (OK)
end


# ISM_DISCONNECT -- Disconnect from the currect channel.

procedure ism_disconnect (send_quit)

int	send_quit

include	"ismfd.com"				# I/O common

begin
	# Send a QUIT message to the server so we shut down the connection.
	if (send_quit == YES)
	    call ism_message ("ximtool", "quit")

	call close (fdin) 		# Close the socket connection.
	call close (fdout)
	fdin = NULL
	fdout = NULL
end


# ISM_MESSAGE -- Send a message to an XImtool named object.  If the object
# is 'ximtool' then just pass the message directly without formatting it.

procedure ism_message (object, message)

char	object[ARB]				#I object name
char	message[ARB]				#I message to send

pointer	sp, msgbuf
int	msglen, olen, mlen, ip 

int	strlen()
bool	streq()

begin
	# Get the message length plus some extra for the braces and padding.
	olen = strlen (object)
	mlen = strlen (message)
	msglen = olen + mlen + 20

	# Allocate and clear the message buffer.
	call smark (sp)
	call salloc (msgbuf, msglen, TY_CHAR)
	call aclrc (Memc[msgbuf], msglen)

	if (streq (object, "ximtool")) {
	    # Just send the message.
	    call strcpy (message, Memc[msgbuf], msglen)
	} else {
	    # Format the message.  We can't use a sprintf here since the
	    # message may be bigger than that allowed by a pargstr().
	    ip = 0
	    call amovc ("send ", Memc[msgbuf+ip], 5)   	; ip = ip + 5
	    call amovc (object,  Memc[msgbuf+ip], olen)	; ip = ip + olen
	    call amovc (" { ",   Memc[msgbuf+ip], 3)   	; ip = ip + 3
	    call amovc (message, Memc[msgbuf+ip], mlen)	; ip = ip + mlen
	    call amovc (" }\0",  Memc[msgbuf+ip], 2)   	; ip = ip + 3
	}
	msglen = strlen (Memc[msgbuf])

	# Now send the message.  The write routine does the strpak().
	call ism_write (Memc[msgbuf], msglen)

	call sfree (sp)
end


# ISM_ALERT -- Send an alert message to XImtool.

procedure ism_alert (text, ok, cancel)

char	text[ARB]				#I warning text
char	ok[ARB]					#i client OK message
char	cancel[ARB]				#i client CANCEL message

pointer	sp, msg

begin
	call smark (sp)
	call salloc (msg, SZ_LINE, TY_CHAR)

	call sprintf (Memc[msg], SZ_LINE, "{%s} {%s} {%s}")
	    call pargstr (text)
	    call pargstr (ok)
	    call pargstr (cancel)

	call ism_message ("alert", Memc[msg])

	call sfree (sp)
end


# ISM_WRITE -- Low-level write of a message to the socket.  Writes exactly
# len bytes to the stream.

procedure ism_write (message, len)

char	message[ARB]				#I message to send
int	len					#I length of message

int	nleft, n, ip
char	msgbuf[SZ_MESSAGE]
int	strlen()

include	"ismfd.com"				# I/O common

errchk	write, flush

begin
	# Pad message with a NULL to terminate it.
	len = strlen (message) + 1
	message[len] = '\0'

	if (mod(len,2) == 1) {
	    len = len + 1
	    message[len] = '\0'
	}

	ip = 1
	nleft = len
	while (nleft > 0) {
	    n = min (nleft, SZ_MESSAGE)
	    call amovc (message[ip], msgbuf, n)
	    if (mode == ISM_BINARY) {
	        call achtcb (msgbuf, msgbuf, n)
                call write (fdout, msgbuf, n / SZB_CHAR)
	    } else
                call write (fdout, msgbuf, n)

	    ip = ip + n
	    nleft = nleft - n
	}
        call flush (fdout)

	if (ISM_DBG) {
	    call eprintf ("ism_write: '%.45s' len=%d mode=%d\n")
	        call pargstr (message);call pargi (len); call pargi (mode)
	}
end


# ISM_READ -- Low-level read from the socket.

int procedure ism_read (message, len)

char	message[ARB]				#O message read
int	len					#O length of message

int	i, n, nleft, read()

include	"ismfd.com"				# I/O common
include "ismcom.com" 				# Interrupt handler variables

errchk	read

begin
	# No data left in the buffer so read from the socket
	if (nbuf == 0) {
	    call aclrc (buffer, SZ_MESSAGE)
	    #call amovkc (EOF, buffer, SZ_MESSAGE)
	    nbuf = 0

            iferr {
		n = read (fdin, message, SZ_MESSAGE)
                if (n < 0) 
	            return (EOF)
            } then {
                if (n < 0) 
	            return (EOF)
        	call xer_reset()
        	call zdojmp (ism_jmp, X_IPC)
            }

	    if (mode == ISM_BINARY) {
	        len = n * SZB_CHAR
	        call achtbc (message, message, len)
	    } else
	       len = n

	    # Save the data read to a local buffer.  Remove any extra 
	    # EOS padding and append an EOF on the string.
	    call amovc (message, buffer, len)
	    if (buffer[len] == EOS && buffer[len-1] == EOS)
		nbuf = len
	    else 
	        nbuf = len + 1
	    buffer[nbuf] = EOF
	}

	for (i=1; buffer[i] != EOS && buffer[i] != EOF && i <= nbuf; i=i+1)
	    message[i] = buffer[i]
	message[i] = '\0'
	len = i				# length of the current message
	nleft = nbuf - i		# nchars left in the buffer

	if (buffer[i] == EOS && buffer[i+1] == EOF) {
	    # That was the last message, force a new read next time we're
	    # called.
	    if (i > 1 && nleft > 1)
	        call amovc (buffer[i+1], buffer, nleft)
	    nbuf = 0
	} else {
	    # More of the message is left in the buffer.
	    if (nleft > 0)
	        call amovc (buffer[i+1], buffer, nleft)
	    nbuf = nleft
	}

	if (ISM_DBG) {
	    message[len] = '\0';
	    call eprintf ("ism_read: len=%d msg='%s'\n")
		call pargi (len); call pargstr(message)
	    call eprintf ("ism_read: nbuf=%d nleft=%d buffer='%s'\n")
		call pargi (nbuf); call pargi(nleft); call pargstr(buffer)
	}

	return (nleft)
end


# ISM_INTRHANDLER -- User-callable interrupt handler so the ISM client code
# doesn't need to know about our internals.

int procedure ism_intrhandler()

extern	ism_zxwhen()

include "ismcom.com" 				# Interrupt handler variables

begin
        call zlocpr (ism_zxwhen, ismepa)
        call xwhen (X_INT, ismepa, old_onint)
        call zsvjmp (ism_jmp, ismstat)

	if (ismstat == OK)
	    return (OK)
	else
	    return (ERR)
end


# ISM_ZXWHEN -- Interrupt handler for the ISM client task.  Branches back
# to ZSVJMP in the user routine to permit shutdown without an error message
# after first disconnecting from the socket.

procedure ism_zxwhen (vex, next_handler)

int     vex             # virtual exception
int     next_handler    # not used

include "ismcom.com" 				# Interrupt handler variables

begin
	call ism_disconnect (YES)
        call xer_reset()
        call zdojmp (ism_jmp, vex)
end


# ISM_ONERROR -- Error exit handler for the interface.  If this is a normal exit
# the shut down quietly, otherwise notify the server.

procedure ism_onerror (status)

int	status					#i not used (req. for ONEXIT)

# Exception handler variables common.
int     ism_errstat
common  /ismecom/ ism_errstat

int	code
char	buf[SZ_LINE], errmsg[SZ_LINE]

int	errget()

include "ismcom.com" 				# Interrupt handler variables

begin
	if (status != OK) {
	    code = errget (errmsg, SZ_LINE)
	    call sprintf (buf, SZ_LINE, "ISM Error, code %d:\n`%s\'")
	        call pargi (status)
	        call pargstr (errmsg)

	    call ism_alert (buf, "", "")
	    call ism_disconnect (YES)
	}
end
