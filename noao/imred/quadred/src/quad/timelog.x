include	<time.h>


# TIMELOG -- Prepend a time stamp to the given string.
#
# For the purpose of a history logging prepend a short time stamp to the
# given string.  Note that the input string is modified.

procedure timelog (str, max_char)

char	str[max_char]		# String to be time stamped
int	max_char		# Maximum characters in string

pointer	sp, time, temp
long	clktime()

begin
	call smark (sp)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (temp, max_char, TY_CHAR)

	call cnvdate (clktime(0), Memc[time], SZ_DATE)
	call sprintf (Memc[temp], max_char, "%s %s")
	    call pargstr (Memc[time])
	    call pargstr (str)
	call strcpy (Memc[temp], str, max_char)

	call sfree (sp)
end
