# PR_CAT2 - Concatenate two strings

procedure pr_cat2 (str1, str2, outstr, maxch)

char	str1[ARB], str2[ARB]		# input strings
char	outstr[ARB]			# output string
int	maxch				# max output chars

begin
	call strcpy (str1, outstr, maxch)
	call strcat (str2, outstr, maxch)
end


# PR_CAT3 - Concatenate three strings

procedure pr_cat3 (str1, str2, str3, outstr, maxch)

char	str1[ARB], str2[ARB], str3[ARB]	# input strings
char	outstr[ARB]			# output string
int	maxch				# max output chars

begin
	call strcpy (str1, outstr, maxch)
	call strcat (str2, outstr, maxch)
	call strcat (str3, outstr, maxch)
end


# PR_CAT4 - Concatenate four strings

procedure pr_cat4 (str1, str2, str3, str4, outstr, maxch)

char	str1[ARB], str2[ARB], str3[ARB], str4[ARB]	# input strings
char	outstr[ARB]					# output string
int	maxch						# max output chars

begin
	call strcpy (str1, outstr, maxch)
	call strcat (str2, outstr, maxch)
	call strcat (str3, outstr, maxch)
	call strcat (str4, outstr, maxch)
end
