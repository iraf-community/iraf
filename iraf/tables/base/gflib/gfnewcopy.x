include <imhdr.h>
include	"gf.h"

# GF_NEWCOPY -- Overwrite the primary header keywords in the user area

procedure gf_newcopy (im, prim)

pointer	im		# i: image descriptor
int	prim		# i: spool file with primary header keywords
#--
pointer	db, old_prim, ext

pointer	gf_find_db()

begin
	db = gf_find_db (im, PARAM_DB)

	if (db == NULL || prim == NULL)
	    return

	call gf_split_ua (im, db, old_prim, ext)

	if (old_prim != NULL)
	    call close (old_prim)

	call gf_upuser (im, prim, WRITE_ONLY)
	call gf_upuser (im, ext,  APPEND)

	# Filter out HISTORY only present in NEW_COPY mode.
	call gf_nohist (im) 

	if (ext != NULL)
	    call close (ext)
end

# GF_NOHIST -- Filter out HISTORY records from im user area w/o unmapping im
procedure gf_nohist(im)

pointer im              # i: image descriptor

#--
pointer ua              # im user area
int     ua_len          # length of image user area
int     imua            # str file view of im user area
int     nonhist         # spool of non-HISTORY keywords
char 	record[SZ_LINE] 

int strlen(), stropen(), open(), getline(), strncmp()

begin
	ua = IM_USERAREA(im)
	ua_len = strlen(Memc[ua])
	imua = stropen(Memc[ua], ua_len, READ_ONLY)
	nonhist = open("nonhist", READ_WRITE, SPOOL_FILE)

	# Capture all records from user area which are *not* HISTORY.
	while(getline(imua, record) != EOF) {
		if (strncmp(record,"HISTORY", 7) != 0) {
			call putline(nonhist, record)
		}
	}

	# Re-open user area for writing.
	call strclose(imua)
	imua = stropen(Memc[ua], ua_len, WRITE_ONLY)

	# Re-write the im user area without HISTORY.
	call seek(nonhist, BOF)
	call fcopyo(nonhist, imua)

	call strclose(imua)
	call close(nonhist)
end
