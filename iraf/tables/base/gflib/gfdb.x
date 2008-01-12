include <imhdr.h>

include	"gf.h"

#* HISTORY *
#* B.Simon	19-Nov-99	Original code
#* B.Simon	08-Nov-00	Removed gf_has_db, no longer used

# The procedures in this file maintain the association between the image
# descriptor and the database descriptor. The database contains extra 
# information needed to support the gf library.

# GF_FIND_DB -- Return cache parameter associated with an image

int procedure gf_find_db (im, param)

pointer	im		# i: image descriptor
int	param		# i: symbolic constant indicating parameter to fetch
#--
include	"gfdb.com"

int	idb, value
string	badparam  "gf_find_db: unrecognized parameter"

begin
	value = 0
	do idb = 1, high {
	    if (imcache[idb] == im) {
		switch (param) {
		case PARAM_DB:
		    value = dbcache[idb]
		case PARAM_GN:
		    value = gncache[idb]
		case PARAM_EXTEND:
		    value = excache[idb]
		case PARAM_INHERIT:
		    value = incache[idb]
		case PARAM_UPDATE:
		    value = upcache[idb] 
		case PARAM_HIST:
		    value = hscache[idb]
		default:
		    call error (1, badparam)
		}
		break
	    }
	}

	return (value)
end

# GF_FORCE_DB -- Set the update flag to yes to force primary header write

procedure gf_force_db (im)

pointer	im		# i: image descriptor
#--
include	"gfdb.com"
int	idb

begin
	do idb = 1, high {
	    if (imcache[idb] == im) {
		upcache[idb] = YES
		return
	    }
	}

	return
end

# GF_REMOVE_DB -- Remove db from cache

procedure gf_remove_db (im, db)

pointer	im		# i: image descriptor
pointer	db		# o: database descriptor
#--
include	"gfdb.com"
int	idb, prev

begin
	prev = 0
	do idb = 1, MAXDB {
	    if (imcache[idb] == im) {
		db = dbcache[idb]
		imcache[idb] = NULL
		dbcache[idb] = NULL
		gncache[idb] = 0
		excache[idb] = NO
		incache[idb] = NO
		upcache[idb] = NO

		if (hscache[idb] != -1) {
			call close(hscache[idb])
			hscache[idb] = -1
		}

		if (idb == high)
		    high = prev
		return
	    }

	    if (imcache[idb] != NULL)
		prev = idb
	}

	db = NULL
	return
end

# GF_RESET_DB -- Reset the image pointer in the cache

procedure gf_reset_db (oldim, im, gn, hist)

pointer	oldim		# i: old image descriptor
pointer	im		# i: replacement image descriptor
int	gn		# i: group number of new image
int     hist

#--
include	"gfdb.com"
int	idb

begin
	do idb = 1, high {
	    if (imcache[idb] == oldim) {
		imcache[idb] = im
		gncache[idb] = gn
		upcache[idb] = NO
		if (hscache[idb] != -1) {
			call close(hscache[idb])
		}
		hscache[idb] = hist
		return
	    }
	}

	return
end

# GF_SETUP_DB -- Set up database of extension keyword names

procedure gf_setup_db (im, db, gn, extend, inherit, hist)

pointer	im		# i: image descriptor
pointer	db		# i: database descriptor
int	gn		# i: group number
int	extend		# i: does fits file have extensions?
int	inherit		# I: do extensions inherit primary header keywords?
int     hist            # i: history spool memory

#--
include	"gfdb.com"
int	idb, jdb

begin
	# Add db to cache

	jdb = 0
	do idb = 1, MAXDB {
	    if (imcache[idb] == im) {
		call gf_freehash (dbcache[idb])
		jdb = idb
		break

	    } else if (imcache[idb] == NULL) {
		if (idb > high)
		    high = idb

		jdb = idb
		break
	    }
	}

	if (jdb == 0) {
	    call error (1, "gf_setup_db: cache full, you forgot gf_unmap?")
	} else {
	    imcache[jdb] = im
	    dbcache[jdb] = db
	    gncache[jdb] = gn
	    excache[jdb] = extend
	    incache[jdb] = inherit
	    upcache[jdb] = NO
	    hscache[jdb] = hist
	}
 
end

