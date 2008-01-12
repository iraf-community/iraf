define	SZ_STACK	25

# SIMILAR -- Return a score base on the similarity between two strings

# This procedure returns a number representing the similarity between 
# two strings. The number is computed by finding the combined length of 
# all the common substrings between the two strings, normalized to a value
# between zero and one hundred.
#
# B.Simon	13-Mar-89	Original

int procedure similar (str1, str2)

char	str1[ARB]	# i: First string
char	str2[ARB]	# i: Second string
#--
int	score, istack, len1, len2, maxch, ic, nc
pointer	stack[4,SZ_STACK]
pointer	sp, word1, word2, s1, s2
pointer	start1, end1, start2, end2, newstart1, newend1, newstart2, newend2

string	overflow  "Stack overflow in procedure similar"

int	strlen()

begin
	# If either string is zero length, return zero as score

	score = 0
	len1 = strlen (str1)
	len2 = strlen (str2)
	if (len1 == 0 || len2 == 0)
	    return (score)

	# Compare the lower case version of the strings

	call smark (sp)
	call salloc (word1, len1, TY_CHAR)
	call salloc (word2, len2, TY_CHAR)

	call strcpy (str1, Memc[word1], len1)
	call strlwr (Memc[word1])

	call strcpy (str2, Memc[word2], len2)
	call strlwr (Memc[word2])

	# The first substrings to compare are the entire strings

	istack = 1
	stack[1,istack] = word1
	stack[2,istack] = word1 + len1 - 1
	stack[3,istack] = word2
	stack[4,istack] = word2 + len2 - 1
	
	# While there are more substrings on the stack

	while (istack > 0) {

	    # Find the longest match between the substrings

	    maxch = 0
	    start1 = stack[1,istack]
	    end1 = stack[2,istack]
	    start2 = stack[3,istack]
	    end2 = stack[4,istack]

	    for (s1 = start1; s1 <= end1 - maxch; s1 = s1 + 1) {

		nc = end1 - s1

		for (s2 = start2; s2 <= end2 - maxch; s2 = s2 + 1) {

		    if (Memc[s1] == Memc[s2]) {

			# Compute the length of the match

			for (ic = 1; 
			     ic <= nc && Memc[s1+ic] == Memc[s2+ic]; 
			     ic = ic + 1)
			    ;

			# If this is the longest match so far, save
			# the length and start and end points

			if (ic > maxch) {
			   maxch = ic
			   newstart1 = s1
			   newstart2 = s2
			   newend1 = s1 + ic - 1
			   newend2 = s2 + ic - 1
			}
			s2 = s2 + ic - 1
		    }
		}
	    }

	    # Pop the stack and push the new substings on the stack

	    istack = istack - 1
	    if (maxch > 0) {
		score = score + 2 * maxch

		if (start1 != newstart1 && start2 != newstart2) {
		    if (istack == SZ_STACK)
			call error (1, overflow)
		    istack = istack + 1
		    stack[1,istack] = start1
		    stack[2,istack] = newstart1 - 1
		    stack[3,istack] = start2
		    stack[4,istack] = newstart2 - 1
		}

		if (end1 != newend1 && end2 != newend2) {
		    if (istack == SZ_STACK)
			call error (1, overflow)
		    istack = istack + 1
		    stack[1,istack] = newend1 + 1
		    stack[2,istack] = end1
		    stack[3,istack] = newend2 + 1
		    stack[4,istack] = end2
		}
	    }
	}

	call sfree (sp)
	return (100 * score / (len1 + len2))
end
