#{  HUB -- Start or stop the Hub

procedure hub (cmd)

string	cmd			{ prompt = "Command"			}
bool	bkg     = yes		{ prompt = "Run in background?"		}
bool	gui     = yes		{ prompt = "Run a GUI window?"		}
bool	verbose = no		{ prompt = "Verbose output?"		}

begin
    string action, command, ch
    bool   verb, do_gui, stat

    action   = cmd
    verb     = verbose
    do_gui   = gui
    command  =  "!" // osfn ("vo$java/app.hub")

    if (action == "start" || action == "on") {
        command = command // " -bg"
	if (!do_gui) {
	    command = command // " -no-gui"
	}
	;
        print (command) | cl(, >& "dev$null")

retry:
	i = 0
	stat = sampHubAccess ()
	for (i=0; stat == no; i = i + 1) {
	    if (verb)
		print ("Waiting for Hub to start ...")
	    sleep (1)

	    if (i > 60)		# only wait a minute to start
		break

	    stat = sampHubAccess ()
	}

	if (stat) {
	    samp on
	    if (verb)
	        print ("Hub and SAMP started")
	} else {
	    printf ("Cannot start or contact SAMP Hub.\n")
prompt:
	    printf ("Would you like to try again (y/n) ? ")

            ch = cl.ukey			# get reply
	    print ("")
            if (ch == "y") {
		goto retry
            } else if (substr(ch,1,4) == "\\015") {
                goto prompt
	    }
	}
	;

    } else if (action == "stop" || action == "off") {
	samp off
        command = command // " -kill"
        print (command) | cl(, >& "dev$null")


    } else if (action == "status") {
        command = command // " -status"
        print (command) | cl()
    }
end
