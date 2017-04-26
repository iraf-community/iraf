#{  ERRTYPE -- Test error types.

procedure errtype (type)

int	type			{ prompt = "Error test type: " 	}

begin
	if (type == 0) 							#  8
	    goto usage_							#  9
									# 10
	switch (type) {							# 11
									# 12
	# SPP task errors.						# 13
	case 1: 			# FPE test			# 14
	    	fpe ()							# 15
	case 2: 			# SEGVIO test			# 16
	    	segvio ()						# 17
	case 3: 			# SPP error() call test		# 18
	    	spperr ()						# 19
									# 20
									# 21
	# CL-generated errors.						# 22
	case 4: 			# non-existant task test 	# 23
	    	nonexist ()						# 24
	case 5: 			# CL error command		# 25
	    	error (type, "cl error() command")			# 26
	case 6: 			# CL div by zero		# 27
		i = 1.0 / 0.0						# 28
	case 7: 			# function error		# 29
		s1 = envget (1)						# 30
	case 8: 			# legal return from script	# 31
	    {								# 32
		print ("simple CL return")				# 33
	    	return							# 34
	    }								# 35
									# 36
	# Grammar tests.						# 37
	case 9: fpe() 			# FPE test w/ no newline	# 38
	case 10:  			# FPE test w/in compound block	# 39
		{ i = 0; fpe(); i = 1;					# 40
		}							# 41
									# 42
	# Pipe tests.							# 43
	case 11:							# 44
		{ print ("fpe") | cl()	# FPE from a piped command	# 45
		}							# 46
	case 12:							# 47
		{ print ("foo") | cl()	# invalid command in a pipe	# 48
		}							# 49
									# 50
	# New features tests.						# 51
#	case -1: 			# Test negative case constant	# 52
#		print ("negative code")					# 53
									# 54
	default:							# 55
		print ("default case reached")				# 56
	}								# 57

	return

usage_:
	print ("1:	fpe				recoverable")
	print ("2	segvio				recoverable")
	print ("3:	spperr				recoverable")
	print ("4:	nonexistant task		recoverable")
	print ("5:	CL error command		recoverable")
	print ("6	CL div by zero			recoverable")
	print ("7:	intrinsic function error	non-recoverable")
	print ("8	CL return			non-error")
	print ("9	FPE test w/ no newline		recoverable - grammar")
	print ("10	FPE test w/in compound block	recoverable - grammar")
	print ("11	FPE from piped command		recoverable")
	print ("12	invalid command in a pipe	internal error")
end									# 74
