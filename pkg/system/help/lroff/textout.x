# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"lroff.h"

# TEXTOUT -- Process a line of text.  Move words from the text buffer into
# the word buffer WBUF, maintaining an array of pointers to the words, until
# an output line has been filled.  Leading whitespace is part of the word,
# if it is the first word on a line (thus we get paragraph indents).
# Thereafter only trailing whitespace is included in the word.  The last word
# on a line gets one trailing space, unless the last char is a period, in which
# case it gets two.  Otherwise whitespace at the end of the input text line is
# stripped.  BREAKLINE is subsequently called to reassemble the words to form
# an output line.
# 
# WBUF is the word buffer (a set of strings separated by EOS markers).  WP is
# a pointer to the next available char in WBUF.  NWORDS is the number of words
# in the buffer.  WORDS is a pointer to the array of word pointers.  We do not
# check for word buffer overflow because the word buffer is allocated large
# enough to accommodate the worst case (the buffer is flushed when an output
# line is filled, which always happens before the buffer overflows).  WCOLS is
# the number of printable characters in the word buffer.  The word buffer
# variables are all stored in the "words" common for use by TEXTOUT and
# BREAKLINE.  Set_wordbuf() must be called upon startup and shutdown to
# allocate/deallocate the word buffer.

procedure set_wordbuf (max_words)

int	max_words		#I output word buffer size

int	word_buffer_size
errchk	malloc

include	"lroff.com"
include	"words.com"

begin
	word_buffer_size = (max_words * 2) + SZ_LINE		# worst case
	if (max_words <= 0 && words != NULL) {
	    call mfree (wbuf, TY_CHAR)
	    call mfree (words, TY_POINTER)
	} else {
	    call malloc (wbuf, word_buffer_size, TY_CHAR)
	    call malloc (words, max_words, TY_POINTER)
	    wp = wbuf
	    nwords = 0
	    wcols = 0
	}
end


# TEXTOUT -- Output a newline delimited line of text.

procedure textout (out, text)

extern	out()
char	text[1]

char	ch
int	ip_save, wcols_save, ip
errchk	breakline
include	"lroff.com"
include	"words.com"

begin
	if (wbuf == NULL || words == NULL)
	    call error (1, "No Lroff word buffer allocated")

	for (ip=1;  text[ip] != EOS;  ) {
	    # Set up descriptors of new word.  Save the input pointer in case
	    # the output line fills and we have to "put the word back".

	    Memi[words+nwords] = wp			# word pointer 
	    ip_save = ip
	    wcols_save = wcols

	    # The following is a nop except at the beginning of a line.
	    for (;  text[ip] == BLANK;  ip=ip+1) {
		Memc[wp] = BLANK
		wp = wp + 1
		wcols = wcols + 1
	    }

	    # Copy the word itself.
	    for (ch=text[ip];  ch != BLANK && ch != EOS;  ch=text[ip]) {
		Memc[wp] = ch
		wp = wp + 1
		if (!INVISIBLE (ch))
		    wcols = wcols + 1
		ip = ip + 1
	    }

	    # And then any trailing whitespace.
	    for (;  text[ip] == BLANK;  ip=ip+1) {
		Memc[wp] = BLANK
		wp = wp + 1
		wcols = wcols + 1
	    }

	    # End of word string.
	    Memc[wp] = EOS
	    wp = wp + 1

	    # If line has been filled, call breakline to format output line
	    # and send it out.  Put word which caused break back for next line.
	    # Do not put word back if it is the first word, or we will have
	    # an infinite loop.
	    if (wcols > (right_margin - left_margin + 1) && nwords > 0) {
		ip = ip_save
		wcols = wcols_save
		wp = Memi[words+nwords]
		call breakline (out, JU)
	    } else
		nwords = nwords + 1
	}

	# Strip trailing whitespace at the end of an input line.  If input
	# line ends with a period, assume it is a sentence and add a blank.
	# Otherwise add a blank to separate words when output line is filled.
	# If a sentence ends within a line, the user is responsible for placing
	# two spaces after the period.

	if (nwords > 0) {
	    for (wp=wp-2;  Memc[wp] == BLANK && wp > wbuf;  wp=wp-1)
		wcols = wcols - 1
	    if (Memc[wp] == '.') {
		wp = wp + 1
		Memc[wp] = BLANK
		wcols = wcols + 1
	    }
	    wp = wp + 1					# point to next avail

	    Memc[wp] = BLANK				# need at least one
	    wp = wp + 1
	    wcols = wcols + 1

	    Memc[wp] = EOS
	    wp = wp + 1
	}
end
