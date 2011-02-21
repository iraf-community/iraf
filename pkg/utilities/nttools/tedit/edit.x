include "command.h"

# EDIT -- Main procedure of table editor

procedure edit (table, columns, silent, rdonly, inplace)

char	table[ARB]		# i: SDAS table name
char	columns[ARB]		# i: list of columns to edit
bool	silent			# i: don't ring bell when error occurs
bool	rdonly			# i: edit table read only
bool	inplace			# i: edit table in place
#--
int	nargs, code
pointer	sp, scr, command, arglist

begin
	call smark (sp)
	call salloc (command, SZ_LINE, TY_CHAR)
	call salloc (arglist, SZ_LINE, TY_CHAR)

	call init_cmd (silent)
	call init_screen (table, columns, rdonly, inplace, scr)
	call help_prompt (scr, NO)

	repeat {
	    call edit_screen (scr)
	    call read_prompt ("Command:", Memc[command], SZ_LINE)
	    call parse_cmd (Memc[command], code, nargs, Memc[arglist], SZ_LINE)

	    if (nargs > 0) {
		switch (code) {
		case TED_ADD:
		    call add_cmd (scr, nargs, Memc[arglist])
		case TED_COPY:
		    call copy_cmd (scr, nargs, Memc[arglist])
		case TED_DELETE:
		    call delete_cmd (scr, nargs, Memc[arglist])
		case TED_EXIT:
		    call exit_cmd (scr, nargs, Memc[arglist])
		    break
		case TED_FIND:
		    call find_cmd (scr, nargs, Memc[arglist])
		case TED_GOTO:
		    call goto_cmd (scr, nargs, Memc[arglist])
		case TED_HELP:
		    call help_cmd (scr, nargs, Memc[arglist])
		case TED_INSERT:
		    call insert_cmd (scr, nargs, Memc[arglist])
		case TED_LOWER:
		    call lower_cmd (scr, nargs, Memc[arglist])
		case TED_NEXT:
		    call next_cmd (scr, nargs, Memc[arglist])
		case TED_QUIT:		    
		    call quit_cmd (scr, nargs, Memc[arglist])
		    break
		case TED_SET:
		    call set_cmd (scr, nargs, Memc[arglist])
		case TED_SUBSTITUTE:
		    call sub_cmd (scr, nargs, Memc[arglist])
		case TED_UPPER:
		    call upper_cmd (scr, nargs, Memc[arglist])
		default:
		    call help_prompt (scr, YES)
		}
	    }
	}

	call end_screen
	call sfree (sp)
end
