# Common for Textout()/Breakline().

pointer	wbuf			# the word buffer, a string buffer
pointer	wp			# pointer to next available char in word buffer
pointer	words			# array of word pointers
int	nwords			# number of words in word buffer
int	wcols			# number of printable columns in word buffer

common	/wrdcom/ wbuf, wp, words, nwords, wcols
