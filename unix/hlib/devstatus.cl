# DEVSTATUS -- Print status info for the named device.  The basic function
# is performed by the hidden builtin _devstatus, but you may with to add
# additional machine dependent function to the script.

procedure devstatus (device)

string	device	{ prompt = "device for which status is desired" }
bool	verbose = no

string	logname, hostname
struct	*devlist
string	dev

begin
	dev = device
	_devstatus (dev)

#	if (verbose) {
#	    # Print UNIX device status, too.
#
#	    devlist = "dev$devices"
#	    while (fscan (devlist, logname, hostname) != EOF) {
#		if (logname == dev) {
#		    print ("!ls -l /dev/", hostname) | cl
#		    break
#		}
#	    }
#	    devlist = ""
#	}
end
