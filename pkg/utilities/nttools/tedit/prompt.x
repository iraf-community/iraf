include "display/curses.h"
include "screen.h"

# BOOL_PROMPT -- Get a yes or no response from the user

bool procedure bool_prompt (msg)

char	msg[ARB]	# i: message to print in the prompt area
#--
int	index
pointer	sp, resp, msg2

string	yorn  "|yes|no|"

int	strdic()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (resp, SZ_FNAME, TY_CHAR)
	call salloc (msg2, SZ_LINE, TY_CHAR)

	# Prompt the user for a response

	call read_prompt (msg, Memc[resp], SZ_FNAME)
	call strlwr (Memc[resp])

	# See if the response is yes or no,
	# if it isn't, keep asking

	index = strdic (Memc[resp], Memc[resp], SZ_FNAME, yorn)
	while (index == 0) {
	    call strcpy ("Please answer yes or no. ", Memc[msg2], SZ_LINE)
	    call strcat (msg, Memc[msg2], SZ_LINE)

	    call read_prompt (Memc[msg2], Memc[resp], SZ_FNAME)
	    call strlwr (Memc[resp])

	    index = strdic (Memc[resp], Memc[resp], SZ_FNAME, yorn)
	}

	# Convert result into boolean value

	call sfree (sp)
	return (index == 1)
end

# CLEAR_PROMPT -- Clear the prompt window

procedure clear_prompt (scr)

pointer	scr		# i: Currently active screen
#--
int	win

int	prompt_window()

begin
	win = prompt_window ()
	call werase (win)

	if (scr != NULL)
	    call focus_window (TED_WINDOW(scr))

end

# ERR1_PROMPT -- Write an error message in the prompt area of the screen

procedure err1_prompt (msg)

char	msg[ARB]	# i: Message to write
#--

begin
	call eprintf ("\r\n")
	call error (1, msg)
end

# ERR2_PROMPT -- Write two error messages in the prompt area of the screen

procedure err2_prompt (msg1, msg2)

char	msg1[ARB]	# i: First message to write
char	msg2[ARB]	# i: Second message to write
#--
pointer	msg

begin
	call salloc (msg, SZ_LINE, TY_CHAR)

	call sprintf (Memc[msg], SZ_LINE, "%s (%s)")
	call pargstr (msg1)
	call pargstr (msg2)

	call eprintf ("\r\n")
	call error (1, Memc[msg])

end

# HELP_PROMPT -- Write help message in prompt area

procedure help_prompt (scr, bell)

pointer	scr		# i: Screen descriptor
int	bell		# i: Ring the bell after printing message?
#--
pointer	sp, exit, msg

string	helpfmt "Type %s quit to leave editor, %s help for help."

begin
	call smark (sp)
	call salloc (exit, SZ_FNAME, TY_CHAR)
	call salloc (msg, SZ_LINE, TY_CHAR)

	call k_eseq ("EXIT_UPDATE", Memc[exit], SZ_FNAME)

	call sprintf (Memc[msg], SZ_LINE, helpfmt)
	call pargstr (Memc[exit])
	call pargstr (Memc[exit])

	call write_prompt (scr, bell, Memc[msg])
	call sfree (sp)
end

# READ_PROMPT -- Read a string from the prompt area of the screen

procedure read_prompt (msg, str, maxch)

char	msg[ARB]	# i: Prompt message
char	str[ARB]	# o: Output string
int	maxch[ARB]	# i: Maximum length of output string
#--
char	blank
int	win

data	blank	  / ' ' /

int	prompt_window()

begin
	# Write message in prompt window

	win = prompt_window ()

	call werase (win)
	call wmove (win, 2, 1)
	call waddstr (win, msg)
	call waddch (win, blank)

	# Read string from prompt window

	call wgetstr (win, str, maxch)
	call werase (win)

end

# WARN1_PROMPT -- Write a warning message in the prompt area of the screen

procedure warn1_prompt (scr, msg)

pointer	scr		# i: Currently active screen
char	msg[ARB]	# i: Message to write
#--

begin
	call write_prompt (scr, YES, msg)
end

# WARN2_PROMPT -- Write two warning messages in the prompt area of the screen

procedure warn2_prompt (scr, msg1, msg2)

pointer	scr		# i: Currently active screen
char	msg1[ARB]	# i: First message to write
char	msg2[ARB]	# i: Second message to write
#--
pointer	sp, msg

begin
	call smark (sp)
	call salloc (msg, SZ_LINE, TY_CHAR)

	call sprintf (Memc[msg], SZ_LINE, "%s (%s)")
	call pargstr (msg1)
	call pargstr (msg2)

	call write_prompt (scr, YES, Memc[msg])

	call sfree (sp)
end

# WRITE_PROMPT -- Write a message in the prompt area of the screen

procedure write_prompt (scr, bell, msg)

pointer	scr		# i: Currently active screen
int	bell		# i: Ring bell after writing message?
char	msg[ARB]	# i: Message to write
#--
int	win

int	prompt_window()

begin
	# Write message in prompt window

	win = prompt_window ()

	call werase (win)
	call wmove (win, 2, 1)
	call waddstr (win, msg)

	# Ring the bell to wake up the user

	if (bell == YES)
	    call ring_bell

	# Restore cursor to original screen position

	if (scr != NULL)
	    call focus_window (TED_WINDOW(scr))

end
