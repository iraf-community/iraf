#
#  CONSOLE -- Small VO Client console/utility tasks


#  VOCDCTL -- VO Client daemon control

procedure t_vocdctl ()

int     fdi, fdo
char	cmd[SZ_FNAME], buf[SZ_FNAME]

int	vx_initVOClient()
int     ndopen(), reopen()
bool	streq()

define	start_ 	99

begin
	call aclrs (cmd, SZ_FNAME)

	call clgstr ("cmd", cmd, SZ_FNAME)

	if (streq (cmd, "start")) {
	    # Initialize the VO Client interface.
start_	    if (vx_initVOClient("console") == ERR) {		
	        call clputi ("status", ERR)
	        call error (0, "Error initializing VO Client")
	        return
	    }

	} else if (streq (cmd, "stop")) {
	    # Close the VO Client interface.  It's possible we might not
	    # already be connected, so open a raw socket and send the text
	    # string directly.

	    iferr {
            	fdi = ndopen ("inet:6200", READ_WRITE)
            	fdo = reopen (fdi, READ_WRITE)

		# Pack and pad the string so we can speak C.
		call strpak ("END ", buf, 4)
            	call write (fdo, buf, 4)
            	call flush (fdo)

            	call close (fdi)
            	call close (fdo)
	    } then {
		call eprintf ("Shutdown failed -- no server?\n")
	    }


	} else if (streq (cmd, "restart")) {
	    # Restart the VO Client interface.
	    call vx_closeVOClient (1)
	    goto start_

	} else {
	    call eprintf ("Invalid command: '%s'\n")
		call pargstr (cmd)
	}
end


#  DBGLEVEL -- Set the VO Client debug level.

procedure t_dbglevel ()

int	level

int	vx_initVOClient(), clgeti()

begin
	# Initialize the VO Client interface.
	if (vx_initVOClient("") == ERR) {		
	    call clputi ("status", ERR)
	    call error (0, "Error initializing VO Client")
	    return
	}

	level = clgeti ("level")
	call vx_dbglevel (level)

	call vx_closeVOClient (0)
end
