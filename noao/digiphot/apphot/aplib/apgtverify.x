# APGTVERIFY -- Print a message in the status line asking the user if they
# really want to quit, returning YES if they really want to quit, NO otherwise.

int procedure apgtverify (ch)

int	ch	# character keystroke command

begin
	if (ch == 'q') {
	    return (YES)
	} else if (ch == 'w') {
	    return (YES)
	} else if (ch == 'n') {
	    return (NO)
	} else {
	    return (NO)
	}

end
