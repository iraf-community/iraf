# CLIO parameters.

int	cl_prtype			# parent process type
pointer	cl_stp				# clcache symbol table pointer
int	cl_stmark			# stmark value for initial table
int	cl_nposargs			# number of $1, $2 type task parameters
int	cl_nextarg			# index into posarg list
pointer	cl_posarg[MAX_POSARGS]		# symtab offsets of positional args
int	ps_status[MAX_PSEUDOFILES]	# for pseudofile drivers
int	cl_npsets			# number of psets for task (>= 1)
int	cl_psetop			# next char in pset name buffer
int	cl_psetindex[MAX_PSETS]		# index of pset names (1 = taskname)
char	cl_psetname[SZ_PSETNAMEBUF]	# char storage for pset names
char	cl_pname[SZ_PNAME]		# handy buffer for param names

common	/clio_com/ cl_prtype, cl_stp, cl_stmark, cl_nposargs, cl_nextarg,
	cl_posarg, ps_status, cl_npsets, cl_psetop, cl_psetindex, cl_psetname,
	cl_pname
