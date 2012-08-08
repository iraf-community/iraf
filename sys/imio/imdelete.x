# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMDELETE -- Delete an image.

procedure imdelete (image)

char	image[ARB]

char	cache[SZ_FNAME], fname[SZ_FNAME], extn[SZ_FNAME]
char	root[SZ_FNAME], src[SZ_FNAME]
int	status, len, ip

int	envgets(), strlen(), iki_access(), imaccess()
bool	streq()

begin
	# Delete a cached version of the file.
	if (envgets ("cache", cache, SZ_PATHNAME) > 0) {
            status = iki_access (image, root, extn, READ_ONLY)
	    len = strlen (root)
	    for (ip = len; ip > 0; ip=ip-1) {
		if (root[ip] == '/') {
		    call strcpy (root[ip+1], root, SZ_FNAME)
		    break
		}
	    }

	    # Make sure the name has the image extension.
	    len = strlen (image)
	    if (! streq (extn, image[len-strlen(extn)+1])) {
		call strcat (".", root, SZ_FNAME)
		call strcat (extn, root, SZ_FNAME)
	    }

	    #  Note that if the file in the cache was added using
	    #  a full path and/or image type extension, it will not
	    #  be found in the cache and deleted.
	    if (status > 0) {
	        call fclookup (cache, image, fname, extn, SZ_FNAME)
		if (fname[1] != EOS) {
                    call fcdelete (cache, fname)

	            call fcsrc (cache, image, src, SZ_FNAME)
	            if (src[1] != EOS) {
	                call fclookup (cache, src, fname, extn, SZ_FNAME)
                        call fcdelete (cache, fname)
                        call fcdelete (cache, src)
	            }
		}
	    }
	}

	if (imaccess (image, READ_ONLY) == YES) {
	    call iki_init()
	    call iki_delete (image)
	}
end
