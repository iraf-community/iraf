include	defs

# gfnarg --- get the next file name from the argument list

   integer function gfnarg (name, state)
   character name (ARB)
   integer state (4)

   integer l
   integer getarg, getlin

   filedes fd
   filedes open

   string in1 "/dev/stdin1"
   string in2 "/dev/stdin2"
   string in3 "/dev/stdin3"

   repeat {

      if (state (1) == 1) {
	 state (1) = 2	      # new state
	 state (2) = 1	      # next argument
	 state (3) = ERR      # current input file
	 state (4) = 0	      # input file count
	 }

      else if (state (1) == 2) {
	 if (getarg (state (2), name, MAXARG) != EOF) {
	    state (1) = 2     # stay in same state
	    state (2) = state (2) + 1  # bump argument count
	    if (name (1) != MINUS) {
	       state (4) = state (4) + 1    # bump input file count
	       return (OK)
	       }
	    else if (name (2) == EOS) {
	       call scopy (in1, 1, name, 1)
	       state (4) = state (4) + 1    # bump input file count
	       return (OK)
	       }
	    else if (name (2) == DIG1 & name (3) == EOS) {
	       call scopy (in1, 1, name, 1)
	       state (4) = state (4) + 1    # bump input file count
	       return (OK)
	       }
	    else if (name (2) == DIG2 & name (3) == EOS) {
	       call scopy (in2, 1, name, 1)
	       state (4) = state (4) + 1    # bump input file count
	       return (OK)
	       }
	    else if (name (2) == DIG3 & name (3) == EOS) {
	       call scopy (in3, 1, name, 1)
	       state (4) = state (4) + 1    # bump input file count
	       return (OK)
	       }

	    else if (name (2) == LETN | name (2) == BIGN) {
	       state (1) = 3	       # new state
	       if (name (3) == EOS)
		  state (3) = STDIN
	       else if (name (3) == DIG1 & name (4) == EOS)
		  state (3) = STDIN1
	       else if (name (3) == DIG2 & name (4) == EOS)
		  state (3) = STDIN2
	       else if (name (3) == DIG3 & name (4) == EOS)
		  state (3) = STDIN3
	       else {
		  state (3) = open (name (3), READ)
		  if (state (3) == ERR) {
		     call putlin (name, ERROUT)
		     call remark (":  can't open.")
		     state (1) = 2
		     }
		  }
	       }
	    else
	       return (ERR)
	    }

	 else
	    state (1) = 4     # EOF state
	 }

      else if (state (1) == 3) {
	 l = getlin (name, state (3))
	 if (l != EOF) {
	    name (l) = EOS
	    state (4) = state (4) + 1  # bump input file count
	    return (OK)
	    }
	 if (fd != ERR & fd != STDIN)
	    call close (state (3))
	 state (1) = 2
	 }

      else if (state (1) == 4) {
	 state (1) = 5
	 if (state (4) == 0) {# no input files
	    call scopy (in1, 1, name, 1)
	    return (OK)
	    }
	 break
	 }

      else if (state (1) == 5)
	 break

      else
	 call error ("in gfnarg:  bad state (1) value.")

      } # end of infinite repeat

   name (1) = EOS
   return (EOF)
   end
