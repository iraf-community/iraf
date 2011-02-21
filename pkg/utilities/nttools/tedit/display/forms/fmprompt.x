# FM_PROMPT -- Get user input from a prompt window
#
# B.Simon	30-Jan-89	Original
# B.Simon	12-Dec-90	Rewritten to use curses

int procedure fm_prompt (win, commands, message)

int	win		# i: Prompt window
char	commands[ARB]	# i: List of commands
char	message[ARB]	# i: Message to print in prompt area
#--
char	newline
int	option
pointer	sp, response, temp

data	newline / '\n' /

int	strdic()

begin
	# Print the message in the window

	call werase (win)
	call wmove (win, 1, 1)
	if (message[1] == EOS) {
	    call waddstr (win, commands)
	    call waddch (win, newline)
	} else {
	    call waddstr (win, message)
	    call waddch (win, newline)
	}

	# Return if no user response is needed

	if (commands[1] == EOS) {
	    call wrefresh (win)
	    return (0)
	}

	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (response, SZ_LINE, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Get the user response

	repeat {
	    call wgetstr (win, Memc[response], SZ_LINE)

	    # Check response against list of commands

	    option = strdic (Memc[response], Memc[temp], SZ_LINE, commands)
	    if (option > 0)
		break

	    # Try again if response was not valid

	    call werase (win)
	    call wmove (win, 1, 1)
	    call waddstr (win , commands)
	    call waddch (win, newline)
	    call ps_beep
	}

	# Return the option number

	call sfree (sp)
	return (option)
end
