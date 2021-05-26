/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "operand.h"
#include "construct.h"
#include "param.h"
#include "task.h"
#include "eparam.h"

int	parse_state;		/* What are we parsing?			*/
int	proc_script;		/* In a procedure script?		*/
struct	pfile *parse_pfile;	/* Where parsed params are added.	*/

int	nextdest[MAX_LOOP];	/* Destinations for NEXT's 		*/
int	brkdest[MAX_LOOP];	/* Destinations for BREAK's 		*/

int	nestlevel = 0;		/* Loop nesting level			*/
int	ncaseval;		/* Number of cases in switch		*/

int	n_oarr;			/* Number of open array indices		*/
int	i_oarr;			/* Current open array index		*/

int	oarr_beg[N_OPEN_ARR];	/* Open index limits.			*/
int	oarr_end[N_OPEN_ARR];
int	oarr_curr[N_OPEN_ARR];	/* Current value for index.		*/
int	imloopset = 0;		/* Loop inited at run time?		*/
int	n_indexes = 0;		/* Number of indexes on stack.		*/

int	maybeindex;		/* Could last constant be index		*/
				/* range?				*/

struct	label	*label1 = NULL; /* Pointer to first top of label list.	*/
int	igoto1 = -1;		/* Head of list of indirect GOTO's	*/

struct	operand	*parlist[MAX_PROC_PARAMS];
struct	param *last_parm;	/* Last parameter before compilation.	*/
int	n_procpar;		/* Number of params in proc stmt.	*/

/* Default initialization of the EDCAP editor command set.
 * Note: these are expected to be reset be the edcap facility at runtime.
 * The source of most of these defaults is the EMACS editor
 */
int	numcommands;		/* number of defined commands		*/

struct edit_commands command[MAX_COMMANDS] = {
	{ EDITOR_ID	,"\0"		,""		},
	{ EDIT_INIT	,"\0"		,"enable"	},
	{ EDIT_TERM	,"\0"		,"disable"	},

	{ MOVE_UP	,"\020"		,"^P"		},
	{ MOVE_DOWN	,"\016"		,"^N"		},
	{ MOVE_RIGHT	,"\006"		,"^F"		},
	{ MOVE_LEFT	,"\002"		,"^B"		},

	{ MOVE_UP	,"\033\133\101" ,"UP ARROW"	},
	{ MOVE_DOWN	,"\033\133\102" ,"DOWN ARROW"	},
	{ MOVE_RIGHT    ,"\033\133\103" ,"RIGHT ARROW"	},
	{ MOVE_LEFT	,"\033\133\104" ,"LEFT ARROW"	},

	{ NEXT_WORD	,"\033\106"	,"ESC-F"	},
	{ NEXT_WORD	,"\033\146"	,"ESC-f"	},
	{ PREV_WORD	,"\033\102"	,"ESC-B"	},
	{ PREV_WORD	,"\033\142"	,"ESC-b"	},
	{ MOVE_EOL	,"\005"		,"^E"		},
	{ MOVE_BOL	,"\001"		,"^A"		},
	{ NEXT_PAGE	,"\026"		,"^V"		},
	{ PREV_PAGE	,"\033\126"	,"ESC-V"	},
	{ PREV_PAGE	,"\033\166"	,"ESC-v"	},
	{ MOVE_START	,"\033\074"	,"ESC-<"	},
	{ MOVE_END	,"\033\076"	,"ESC->"	},

	{ SET_FWD	,"\000"		,"undefined"	},
	{ SET_AFT	,"\000"		,"undefined"	},
	{ TOGGLE_DIR	,"\000"		,"undefined"	},

	{ DEL_LEFT	,"\177"		,"DEL"		},
	{ DEL_LEFT	,"\010"		,"^H or BS"	},
	{ DEL_CHAR	,"\004"		,"^D"		},
	{ DEL_WORD	,"\033\104"	,"ESC-D"	},
	{ DEL_WORD	,"\033\144"	,"ESC-d"	},
	{ DEL_LINE	,"\013"		,"^K"		},
	{ UNDEL_CHAR	,"\033\004"	,"ESC-^D"	},
	{ UNDEL_WORD	,"\033\027"	,"ESC-^W"	},
	{ UNDEL_LINE	,"\033\013"	,"ESC-^K"	},

	{ FIND_FWD	,"\033\123"	,"ESC-S"	},
	{ FIND_FWD	,"\033\163"	,"ESC-s"	},
	{ FIND_AFT	,"\033\122"	,"ESC-R"	},
	{ FIND_AFT	,"\033\162"	,"ESC-r"	},
	{ FIND_NEXT	,"\000"		,"undefined"	},

	{ GET_HELP	,"\033\077"	,"ESC-?"	},
	{ REPAINT	,"\014"		,"^L"		},
	{ EXIT_UPDATE	,"\032"		,"^Z"		},
	{ EXIT_NOUPDATE ,"\003"		,"^C"		},

	{ NEXT_LINE     ,"\000"		,"undefined"	},
	{ NOMORE_COMMANDS ,"\0"		,""		}
};

/* Names of the editor commands, used for edcap interpretation and showhelp.
 */
char	*cmdnames[MAX_COMMANDS] = {
	"EDITOR_ID", "EDIT_INIT", "EDIT_TERM",
	"MOVE_UP", "MOVE_DOWN", "MOVE_RIGHT", "MOVE_LEFT", "NEXT_WORD",
	"PREV_WORD", "MOVE_EOL", "MOVE_BOL", "NEXT_PAGE", "PREV_PAGE",
	"MOVE_START", "MOVE_END", "SET_FWD", "SET_AFT", "TOGGLE_DIR",
	"DEL_LEFT", "DEL_CHAR", "DEL_WORD", "DEL_LINE", "UNDEL_CHAR",
	"UNDEL_WORD", "UNDEL_LINE", "FIND_FWD", "FIND_AFT", "FIND_NEXT",
	"GET_HELP", "REPAINT", "EXIT_UPDATE", "EXIT_NOUPDATE",
	"NEXT_LINE", "NOMORE_COMMANDS"
};
