## VOClient client-side API bindings.
## Process this file with automake to produce Makefile.in


# VOClient libraries.
lib_LTLIBRARIES      = libvoclient.la

libvoclient_la_SOURCES   = \
	vocDAL.c vocDAL_f77.c vocDAL_spp.c \
	vocLib.c vocLib_f77.c vocLib_spp.c \
	vocRegistry.c vocRegistry_f77.c vocRegistry_spp.c \
	vocSesame.c vocSesame_f77.c vocSesame_spp.c \
	vocSkybot.c vocSkybot_f77.c vocSkybot_spp.c \
	vocUtil_f77.c vocUtil_spp.c vocMsg.c

libvoclient_la_LIBADD    = @LTLIBOBJS@


## VOClient includes.
include_HEADERS = VOClient.h


## Language-independent SWIG interface to VOClient.
#SWIG_SOURCES = VOClient.i

# Python binding.
#BUILT_SOURCES = vocWrapPython.c
#pkgpython_PYTHON = voclient.py
CLEANFILES = voclient* SWIG* php_*.h

#pkgpyexec_LTLIBRARIES = _voclient.la
#_voclient_la_SOURCES = vocWrapPython.c $(SWIG_SOURCES)
#_voclient_la_CPPFLAGS = $(SWIG_PYTHON_CPPFLAGS)
#_voclient_la_LDFLAGS = -module
#_voclient_la_LIBADD = libvoclient.la

#vocWrapPython.c : $(SWIG_SOURCES)
#	$(SWIG) $(SWIG_PYTHON_OPT) -o $@ $<

java_wrap:  VOClient.i
	$(SWIG) -java -o vocWrapJava.c VOClient.i

