include	<pmset.h>
include	"ace.h"
include	"cat.h"
include	"objs.h"
include	"grow.h"


procedure grow (grw, cat, objmask, logfd)

pointer	grw			#I Grow parameter structure
pointer	cat			#I Catalog of objects
pointer	objmask			#I Object mask
int	logfd			#I Logfile

int	ngrow			#I Number of pixels to grow
real	agrow			#I Area factor grow

int	i, j, nc, nl, m, n
pointer	sp, v, bufs, obuf
pointer	buf1, buf2, buf3, obj

int	grow1(), grow2(), grow3(), andi(), ori(), noti()
pointer	cathead(), catnext()

begin
	call grw_pars ("open", "", grw)
	ngrow = GRW_NGROW(grw)
	agrow = GRW_AGROW(grw)

	if (ngrow < 1 && agrow <= 1.)
	    return

	if (logfd != NULL) {
	    call fprintf (logfd, "  Grow objects: ngrow = %d, agrow = %g\n")
		call pargi (ngrow)
		call pargr (agrow)
	}

	call smark (sp)
	call salloc (v, PM_MAXDIM, TY_LONG)

	call pm_gsize (objmask, i, Meml[v], n)
	nc = Meml[v]; nl = Meml[v+1]
	Meml[v] = 1

	call salloc (bufs, 3, TY_POINTER)
	do i = 1, 3
	    call salloc (Memi[bufs+i-1], nc, TY_INT)
	call salloc (obuf, nc, TY_INT)

	for (obj=cathead(cat); obj!=NULL; obj=catnext(cat,obj)) {
	    if (GROWN(obj))
		next
	    UNSETFLAG (obj, OBJ_EVAL)
	    OBJ_NDETECT(obj) = OBJ_NPIX(obj)
	}

	do j = 1, ngrow {
	    m = 0
	    buf2 = NULL; buf3 = NULL
	    do i = 1, nl {
		buf1 = buf2
		buf2 = buf3
		buf3 = NULL

		if (i != 1 && buf1 == NULL) {
		    Meml[v+1] = i - 1
		    buf1 = Memi[bufs+mod(Meml[v+1],3)]
		    call pmglpi (objmask, Meml[v], Memi[buf1], 0, nc, 0)
		}
		if (buf2 == NULL) {
		    Meml[v+1] = i
		    buf2 = Memi[bufs+mod(Meml[v+1],3)]
		    call pmglpi (objmask, Meml[v], Memi[buf2], 0, nc, 0)
		}
		if (i != nl && buf3 == NULL) {
		    Meml[v+1] = i+1
		    buf3 = Memi[bufs+mod(Meml[v+1],3)]
		    call pmglpi (objmask, Meml[v], Memi[buf3], 0, nc, 0)
		}

		if (i == 1)
		    n = grow1 (cat, i, Memi[buf2], Memi[buf3],
			Memi[obuf], nc, nl)
		else if (i == nl)
		    n = grow3 (cat, i, Memi[buf1], Memi[buf2],
			Memi[obuf], nc, nl)
		else
		    n = grow2 (cat, i, Memi[buf1], Memi[buf2], Memi[buf3],
			Memi[obuf], nc, nl)

		if (n > 0) {
		    Meml[v+1] = i
		    call pmplpi (objmask, Meml[v], Memi[obuf], 0, nc, PIX_SRC) 
		    m = m + n
		}
	    }

	    n = 0
	    for (obj=cathead(cat); obj!=NULL; obj=catnext(cat,obj)) {
		if (GROWN(obj))
		    next
		if (real (OBJ_NPIX(obj)) / OBJ_NDETECT(obj) >= agrow)
		    SETFLAG (obj, OBJ_GROW)
		else
		    n = n + 1
	    }

	    if (n == 0 || m == 0)
		break
	}

	if (n != 0) {
	    for (obj=cathead(cat); obj!=NULL; obj=catnext(cat,obj)) {
		if (GROWN(obj))
		    next
		SETFLAG (obj, OBJ_GROW)
	    }
	}
		
	call sfree (sp)
end


int procedure grow1 (cat, line, in2, in3, out, nc, nl)

pointer	cat		#I Catalog
int	line		#I Line
int	in2[nc]		#I Current line
int	in3[nc]		#I Next line
int	out[nc]		#I Output line
int	nc, nl		#I Dimension of image

int	i, j, n, id, id0, id1, num1, andi()
bool	grow
pointer	objs, obj, obj1

begin	
	objs = CAT_OBJS(cat) - 1
	obj1 = NULL
	n = 0
	do i = 1, nc {
	    id0 = in2[i]
	    if (id0 != 0 && MNOTSPLIT(id0)) {
		out[i] = id0
		next
	    }

	    id = 0
	    j = i - 1
	    if (i > 1) {
		id1 = in2[j]
		num1 = MNUM(id1)
		if (num1 >= NUMSTART) {
		    if (MNOTSPLIT(id1)) {
			if (obj1 == NULL) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			} else if (OBJ_NUM(obj1) != num1) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			}
			if (grow) {
			    if (id == 0) {
				id = id1
				obj = obj1
			    } else if (id != id1) {
				if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				    id = id1
				    obj = obj1
				}
			    }
			}
		    }
		}
		id1 = in3[j]
		num1 = MNUM(id1)
		if (num1 >= NUMSTART) {
		    if (MNOTSPLIT(id1)) {
			if (obj1 == NULL) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			} else if (OBJ_NUM(obj1) != num1) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			}
			if (grow) {
			    if (id == 0) {
				id = id1
				obj = obj1
			    } else if (id != id1) {
				if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				    id = id1
				    obj = obj1
				}
			    }
			}
		    }
		}
	    }
	    id1 = in3[i]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    j = i + 1
	    if (i < nc) {
		id1 = in2[j]
		num1 = MNUM(id1)
		if (num1 >= NUMSTART) {
		    if (MNOTSPLIT(id1)) {
			if (obj1 == NULL) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			} else if (OBJ_NUM(obj1) != num1) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			}
			if (grow) {
			    if (id == 0) {
				id = id1
				obj = obj1
			    } else if (id != id1) {
				if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				    id = id1
				    obj = obj1
				}
			    }
			}
		    }
		}
		id1 = in3[j]
		num1 = MNUM(id1)
		if (num1 >= NUMSTART) {
		    if (MNOTSPLIT(id1)) {
			if (obj1 == NULL) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			} else if (OBJ_NUM(obj1) != num1) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			}
			if (grow) {
			    if (id == 0) {
				id = id1
				obj = obj1
			    } else if (id != id1) {
				if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				    id = id1
				    obj = obj1
				}
			    }
			}
		    }
		}
	    }

	    if (id == 0)
		out[i] = in2[i]
	    else {
		out[i] = id
		OBJ_NPIX(obj) = OBJ_NPIX(obj) + 1
		n = n + 1
	    }
	}

	return (n)
end


int procedure grow2 (cat, line, in1, in2, in3, out, nc, nl)

pointer	cat		#I Catalog
int	line		#I Line
int	in1[nc]		#I Previous line
int	in2[nc]		#I Current line
int	in3[nc]		#I Next line
int	out[nc]		#I Output line
int	nc, nl		#I Dimension of image

int	i, j, n, id, id0, id1, num1, andi()
bool	grow
pointer	objs, obj, obj1

begin	
	objs = CAT_OBJS(cat) - 1
	obj1 = NULL
	n = 0
	do i = 2, nc-1 {
	    id0 = in2[i]
	    if (id0 != 0 && MNOTSPLIT(id0)) {
		out[i] = id0
		next
	    }

	    id = 0
	    j = i - 1
	    id1 = in1[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in2[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in3[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in1[i]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in3[i]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    j = i + 1
	    id1 = in1[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in2[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in3[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }

	    if (id == 0)
		out[i] = in2[i]
	    else {
		out[i] = id
		OBJ_NPIX(obj) = OBJ_NPIX(obj) + 1
		n = n + 1
	    }
	}

	# First pixel
	id0 = in2[1]
	if (id0 != 0 && MNOTSPLIT(id0))
	    out[1] = id0
	else {
	    id = 0
	    id1 = in1[1]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in3[1]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in1[2]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in2[2]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in3[2]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }

	    if (id == 0)
		out[1] = in2[1]
	    else {
		out[1] = id
		OBJ_NPIX(obj) = OBJ_NPIX(obj) + 1
		n = n + 1
	    }
	}

	# Last pixel
	id0 = in2[nc]
	if (id0 != 0 && MNOTSPLIT(id0))
	    out[nc] = id0
	else {
	    id = 0
	    j = nc - 1
	    id1 = in1[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in2[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in3[j]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in1[nc]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    id1 = in3[nc]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }

	    if (id == 0)
		out[nc] = in2[nc]
	    else {
		out[nc] = id
		OBJ_NPIX(obj) = OBJ_NPIX(obj) + 1
		n = n + 1
	    }
	}

	return (n)
end


int procedure grow3 (cat, line, in1, in2, out, nc, nl)

pointer	cat		#I Catalog
int	line		#I Line
int	in1[nc]		#I Previous line
int	in2[nc]		#I Current line
int	out[nc]		#I Output line
int	nc, nl		#I Dimension of image

int	i, j, n, id, id0, id1, num1, andi()
bool	grow
pointer	objs, obj, obj1

begin	
	objs = CAT_OBJS(cat) - 1
	obj1 = NULL
	n = 0
	do i = 1, nc {
	    id0 = in2[i]
	    if (id0 != 0 && MNOTSPLIT(id0)) {
		out[i] = id0
		next
	    }

	    id = 0
	    j = i - 1
	    if (i > 1) {
		id1 = in1[j]
		num1 = MNUM(id1)
		if (num1 >= NUMSTART) {
		    if (MNOTSPLIT(id1)) {
			if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			} else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			}
			if (grow) {
			    if (id == 0) {
				id = id1
				obj = obj1
			    } else if (id != id1) {
				if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				    id = id1
				    obj = obj1
				}
			    }
			}
		    }
		}
		id1 = in2[j]
		num1 = MNUM(id1)
		if (num1 >= NUMSTART) {
		    if (MNOTSPLIT(id1)) {
			if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			} else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			}
			if (grow) {
			    if (id == 0) {
				id = id1
				obj = obj1
			    } else if (id != id1) {
				if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				    id = id1
				    obj = obj1
				}
			    }
			}
		    }
		}
	    }
	    id1 = in1[i]
	    num1 = MNUM(id1)
	    if (num1 >= NUMSTART) {
		if (MNOTSPLIT(id1)) {
		    if (obj1 == NULL) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    } else if (OBJ_NUM(obj1) != num1) {
			obj1 = Memi[objs+num1]
			grow = NOTGROWN(obj1)
		    }
		    if (grow) {
			if (id == 0) {
			    id = id1
			    obj = obj1
			} else if (id != id1) {
			    if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				id = id1
				obj = obj1
			    }
			}
		    }
		}
	    }
	    j = i + 1
	    if (i < nc) {
		id1 = in1[j]
		num1 = MNUM(id1)
		if (num1 >= NUMSTART) {
		    if (MNOTSPLIT(id1)) {
			if (obj1 == NULL) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			} else if (OBJ_NUM(obj1) != num1) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			}
			if (grow) {
			    if (id == 0) {
				id = id1
				obj = obj1
			    } else if (id != id1) {
				if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				    id = id1
				    obj = obj1
				}
			    }
			}
		    }
		}
		id1 = in2[j]
		num1 = MNUM(id1)
		if (num1 >= NUMSTART) {
		    if (MNOTSPLIT(id1)) {
			if (obj1 == NULL) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			} else if (OBJ_NUM(obj1) != num1) {
			    obj1 = Memi[objs+num1]
			    grow = NOTGROWN(obj1)
			}
			if (grow) {
			    if (id == 0) {
				id = id1
				obj = obj1
			    } else if (id != id1) {
				if (OBJ_FLUX(obj) < OBJ_FLUX(obj1)) {
				    id = id1
				    obj = obj1
				}
			    }
			}
		    }
		}
	    }

	    if (id == 0)
		out[i] = in2[i]
	    else {
		out[i] = id
		OBJ_NPIX(obj) = OBJ_NPIX(obj) + 1
		n = n + 1
	    }
	}

	return (n)
end
