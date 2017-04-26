include	defs

# amatch --- (non-recursive) look for match starting at lin (from)

   integer function amatch (lin, from, pat, tagbeg, tagend)
   character lin (MAXLINE), pat (MAXPAT)
   integer from, tagbeg (10), tagend (10)

   integer i, j, offset, stack
   integer omatch, patsiz

   for (i = 1; i <= 10; i = i + 1) {
      tagbeg (i) = 0
      tagend (i) = 0
      }
   tagbeg (1) = from
   stack = 0
   offset = from      # next unexamined input character
   for (j = 1; pat (j) != EOS; j = j + patsiz (pat, j))
      if (pat (j) == CLOSURE) {      # a closure entry
	 stack = j
	 j = j + CLOSIZE      # step over CLOSURE
	 for (i = offset; lin (i) != EOS; )	 # match as many as
	    if (omatch (lin, i, pat, j) == NO)	 # possible
	       break
	 pat (stack + COUNT) = i - offset
	 pat (stack + START) = offset
	 offset = i	 # character that made us fail
	 }
      else if (pat (j) == START_TAG) {
	 i = pat (j + 1)
	 tagbeg (i + 1) = offset
	 }
      else if (pat (j) == STOP_TAG) {
	 i = pat (j + 1)
	 tagend (i + 1) = offset
	 }
      else if (omatch (lin, offset, pat, j) == NO) {  # non-closure
	 for ( ; stack > 0; stack = pat (stack + PREVCL))
	    if (pat (stack + COUNT) > 0)
	       break
	 if (stack <= 0) {	# stack is empty
	    amatch = 0	    # return failure
	    return
	    }
	 pat (stack + COUNT) = pat (stack + COUNT) - 1
	 j = stack + CLOSIZE
	 offset = pat (stack + START)  +  pat (stack + COUNT)
	 }
      # else omatch succeeded

   amatch = offset
   tagend (1) = offset
   return      # success
   end
