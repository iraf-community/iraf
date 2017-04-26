include "filetype.h"

# FILETYPE -- Determine filetype of file

int procedure filetype (file)

char	file	# i: file name
#--
int     flag

int     is_image()

begin
	# Is_image is in the selector sublibrary of tbtables
	# and is the recommended procedure for determining file
	# type

        switch (is_image(file)) {
        case ERR:
            flag = UNKNOWN_FILE
        case NO:
            flag = TABLE_FILE
        case YES:
            flag = IMAGE_FILE
        }

        return (flag)
end
