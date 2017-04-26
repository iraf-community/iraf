# Class common.  
int     cl_nclass                          	# number of defined functions
int     cl_table[LEN_CLASS,MAX_CLASSES]        	# class table
char    cl_names[SZ_CLNAME,MAX_CLASSES]      	# class names
common  /class_com/ cl_nclass, cl_table, cl_names

