# TABLE.H -- Tedit table descriptor

define	TED_TABLEN	13			# table descriptor length

define	TED_READONLY   	Memi[$1]		# is table read only?
define	TED_NEWTAB	Memi[$1+1]		# is this a new table?
define	TED_INPLACE	Memi[$1+2]		# edit table in place?
define	TED_ALLCOLS	Memi[$1+3]		# editing all columns?
define	TED_DIRTY	Memi[$1+4]		# has table been modified?
define	TED_TABPTR     	Memi[$1+5]		# table pointer
define	TED_NAMEPTR	Memi[$1+6]		# ptr to original table name
define	TED_NCOLS      	Memi[$1+7]		# number of columns
define	TED_LABWIDTH	Memi[$1+8]		# label width
define	TED_LABHEIGHT	Memi[$1+9]		# label height
define	TED_COLARY	Memi[$1+10]		# array of column pointers
define	TED_TYPARY	Memi[$1+11]		# array of column types
define	TED_LENARY	Memi[$1+12]		# array of column lengths

define	TED_TABNAME	Memc[TED_NAMEPTR($1)]		# original table name
define	TED_COLPTR	Memi[TED_COLARY($1)+($2)-1]	# column pointer
define	TED_COLTYPE	Memi[TED_TYPARY($1)+($2)-1]	# column type 
define	TED_COLLEN	Memi[TED_LENARY($1)+($2)-1]	# column length
