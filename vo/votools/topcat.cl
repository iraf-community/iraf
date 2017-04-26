#{  TOPCAT -- Start or stop the TOPCAT app.

procedure topcat (cmd)

string	cmd			{ prompt = "Command"			}
bool	bkg     = yes		{ prompt = "Run in background?"		}
bool	verbose = no		{ prompt = "Print actions?"		}

begin
    string action, command, ch
    bool   verb


    if ($nargs > 0) 
      action   =  cmd
    else
      action   =  "start"
    verb       =  verbose
    command    =  "!" // osfn ("vo$java/app.topcat")


    if (action == "stop" || action == "off") {
        if (verb)
            printf ("Stopping Topcat .... ")
        command = command // " -kill"
        print (command) | cl(, >& "dev$null")


    } else if (action == "status") {
        command = command // " -status"
        print (command) | cl()

    } else {
	# Default is to start the app
        if (verb)
            printf ("Starting Topcat .... ")
        command = command // " -bg"
        print (command) | cl(, >& "dev$null")
    }

    if (verb)
	printf ("\n")
end
