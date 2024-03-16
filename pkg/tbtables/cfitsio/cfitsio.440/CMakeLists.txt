#==============================================================================
# CFITSIO CMakeLists.txt
#==============================================================================

# Ensure that build directory is not the source directory:
IF(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
	message(FATAL_ERROR
		"Please create a separate directory for building CFITSIO."
		)
ENDIF()

CMAKE_MINIMUM_REQUIRED(VERSION 3.1)

# Suppress warning about WIN32 no longer being defined on Cygwin:
SET(CMAKE_LEGACY_CYGWIN_WIN32 0)

PROJECT(CFITSIO
	DESCRIPTION "Library of C and Fortran subroutines for reading and writing data files in FITS data format."
	LANGUAGES C CXX
	)

# Define project version
SET(${PROJECT_NAME}_MAJOR 4)
SET(${PROJECT_NAME}_MINOR 4)
SET(${PROJECT_NAME}_MICRO 0)
SET(${PROJECT_NAME}_VERSION ${${PROJECT_NAME}_MAJOR}.${${PROJECT_NAME}_MINOR}.${${PROJECT_NAME}_MICRO})
SET(LIB_NAME cfitsio)

# Allow @rpath token in target install name on Macs.
# See "cmake --help-policy CMP0042" for more information.
IF(POLICY CMP0042)
  CMAKE_POLICY(SET CMP0042 NEW)
ENDIF()

INCLUDE(CheckLibraryExists)
INCLUDE(CheckFunctionExists)
INCLUDE(CheckSymbolExists)
INCLUDE(CheckCSourceCompiles)

#==============================================================================
# Build options:
#==============================================================================

# Allow the developer to select if Dynamic or Static libraries are built
OPTION(BUILD_SHARED_LIBS "Build Shared Libraries" ON)

# Allow curl dependency to be disabled with "-DUSE_CURL=OFF":
OPTION(USE_CURL "USE_CURL" ON)

OPTION(USE_PTHREADS "Thread-safe build (using pthreads)" OFF)

OPTION(TESTS "Build test programs Testprog and cookbook" OFF)

OPTION(UTILS "Build FPack, Funpack, Fitscopy, and Fitsverify executables" OFF)

OPTION(USE_SSE2 "Enable use of instructions in the SSE2 extended instruction set" OFF)

OPTION(USE_SSSE3 "Enable use of instructions in the SSSE3 extended instruction set" OFF)

IF(NOT WIN32)
    OPTION(USE_BZIP2 "Enable bzip2 support" OFF)
ENDIF()

#==============================================================================

FIND_PACKAGE(ZLIB REQUIRED)

IF(BUILD_SHARED_LIBS)
  SET (LIB_TYPE SHARED)
ELSE()
  SET (LIB_TYPE STATIC)
ENDIF(BUILD_SHARED_LIBS)

IF(NOT WIN32)
   # Find curl library, for HTTPS support:
   IF(USE_CURL)
      FIND_PACKAGE(CURL)
      IF(CURL_FOUND)
         ADD_DEFINITIONS(-DCFITSIO_HAVE_CURL)
      ENDIF()
   ENDIF()
ENDIF(NOT WIN32)

IF(USE_PTHREADS)
   FIND_PACKAGE(Threads REQUIRED)
   ADD_DEFINITIONS(-D_REENTRANT)
ENDIF(USE_PTHREADS)

IF(USE_BZIP2)
   FIND_PACKAGE(BZip2 REQUIRED)
   ADD_DEFINITIONS(-DHAVE_BZIP2=1)
ENDIF(USE_BZIP2)

# Math library (not available in MSVC or MINGW)
IF(MSVC OR MINGW)
   SET(M_LIB "")
ELSE()
   FIND_LIBRARY(M_LIB m)
ENDIF()

# Support for remote file drivers is not implemented for native Windows:
IF (NOT MSVC)

    # Find library needed for gethostbyname:
    CHECK_FUNCTION_EXISTS("gethostbyname" CMAKE_HAVE_GETHOSTBYNAME)
    IF (NOT CMAKE_HAVE_GETHOSTBYNAME)
        CHECK_LIBRARY_EXISTS("nsl" "gethostbyname" "" CMAKE_HAVE_GETHOSTBYNAME)
    ENDIF ()

    # Find library needed for connect:
    CHECK_FUNCTION_EXISTS("connect" CMAKE_HAVE_CONNECT)
    IF (NOT CMAKE_HAVE_CONNECT)
        CHECK_LIBRARY_EXISTS("socket" "connect" "" CMAKE_HAVE_CONNECT)
    ENDIF ()

    # Define HAVE_NET_SERVICES if gethostbyname & connect were found:
    IF (CMAKE_HAVE_GETHOSTBYNAME AND CMAKE_HAVE_CONNECT)
        ADD_DEFINITIONS(-DHAVE_NET_SERVICES)
        CHECK_SYMBOL_EXISTS(fmemopen "stdio.h" HAVE_FMEMOPEN)
        IF(HAVE_FMEMOPEN)
            ADD_DEFINITIONS(-DHAVE_FMEMOPEN)
        ENDIF()
    ENDIF()

ENDIF()

IF(MSVC OR BORLAND)
    # Define
    ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)
    # Need an empty unistd.h in MSVC for flex-generated eval_l.c:
    FILE(WRITE ${CMAKE_SOURCE_DIR}/unistd.h "")
    INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR})
ENDIF()

IF(BORLAND)
    # Suppress spurious Borland compiler warnings about "Suspicious
    # pointer arithmetic", "Possibly incorrect assignment", and
    # "Comparing signed and unsigned values".
    ADD_DEFINITIONS(-w-spa)
    ADD_DEFINITIONS(-w-pia)
    ADD_DEFINITIONS(-w-csu)
ENDIF()

IF(USE_SSE2)
    IF("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR
       "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR
       "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
        ADD_COMPILE_OPTIONS(-msse2)
    ENDIF()
    IF(MSVC)
        ADD_DEFINITIONS(-D__SSE2__=1)
    ENDIF()
ENDIF()

IF(USE_SSSE3)
    IF("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR
       "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR
       "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
        ADD_COMPILE_OPTIONS(-mssse3)
    ENDIF()
    IF(MSVC)
        ADD_DEFINITIONS(-D__SSE2__=1 -D__SSSE3__=1)
    ENDIF()
ENDIF()

# Test for the unix ftruncate function
CHECK_SYMBOL_EXISTS(ftruncate "unistd.h" HAVE_FTRUNCATE)
IF(HAVE_FTRUNCATE)
    ADD_DEFINITIONS(-DHAVE_UNISTD_H -DHAVE_FTRUNCATE)
ENDIF()

# Check is System V IPC is supported on this machine
CHECK_C_SOURCE_COMPILES("
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
int main() {
  shmat(0,0,0);
  shmdt(0);
  shmget(0, 0, 0);
  semget(0, 0, 0);
  return 0;
}" HAVE_SHMEM_SERVICES)
IF(HAVE_SHMEM_SERVICES)
    ADD_DEFINITIONS(-DHAVE_SHMEM_SERVICES)

    # Some systems define flock_t, for others we have to define it ourselves
    CHECK_C_SOURCE_COMPILES("
    #include <sys/flock.h>
    int main() {
      flock_t filler;
      return 0;
    }" HAVE_FLOCK_T)
    IF(HAVE_FLOCK_T)
        ADD_DEFINITIONS(-DHAVE_FLOCK_T)
    ENDIF()

    # Check union semun
    CHECK_C_SOURCE_COMPILES("
    #include <sys/ipc.h>
    #include <sys/shm.h>
    #include <sys/sem.h>
    int main() {
      union semun filler;
      return 0;
    }" HAVE_UNION_SEMUN)
    IF(HAVE_UNION_SEMUN)
        ADD_DEFINITIONS(-DHAVE_UNION_SEMUN)
    ENDIF()
ENDIF()

IF(WIN32 AND BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-Dcfitsio_EXPORTS)
ENDIF()

#==============================================================================
# CFITSIO source code:
#==============================================================================

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}")
set (LIB_DESTINATION "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}")
set (INCLUDE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/include/")
set (BIN_DESTINATION "${CMAKE_INSTALL_PREFIX}/bin")

SET(H_FILES fitsio.h fitsio2.h longnam.h)

SET(SRC_FILES
    buffers.c cfileio.c checksum.c
    drvrfile.c drvrmem.c drvrnet.c drvrsmem.c
    editcol.c edithdu.c eval_f.c eval_l.c eval_y.c
    f77_wrap1.c f77_wrap2.c f77_wrap3.c f77_wrap4.c
    fits_hcompress.c fits_hdecompress.c fitscore.c
    getcol.c getcolb.c getcold.c getcole.c getcoli.c getcolj.c getcolk.c
    getcoll.c getcols.c getcolsb.c getcolui.c getcoluj.c getcoluk.c
    getkey.c group.c grparser.c histo.c imcompress.c iraffits.c modkey.c
    pliocomp.c
    putcol.c putcolb.c putcold.c putcole.c putcoli.c putcolj.c putcolk.c
    putcoll.c putcols.c putcolsb.c putcolu.c putcolui.c putcoluj.c putcoluk.c
    putkey.c quantize.c region.c ricecomp.c scalnull.c simplerng.c swapproc.c
    wcssub.c wcsutil.c zcompress.c zuncompress.c
)

# For future modifications:

# drvrsmem.c is only usable if HAVE_SHMEM_SERVICES is defined:
#drvrsmem.c

# drvrgsiftp.c is only usable if HAVE_NET_SERVICES & HAVE_GSIFTP are defined:
#drvrgsiftp.c

#==============================================================================
# Add libraries:
#==============================================================================
ADD_LIBRARY(${LIB_NAME} ${LIB_TYPE} ${H_FILES} ${SRC_FILES})
# Add an alias definition to handle export namespace for
# add_subdirectory():
ADD_LIBRARY(${LIB_NAME}::${LIB_NAME} ALIAS ${LIB_NAME})

TARGET_INCLUDE_DIRECTORIES(${LIB_NAME} PUBLIC
    $<BUILD_INTERFACE:${CFITSIO_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
    $<INSTALL_INTERFACE:include>
)
# Math:
TARGET_LINK_LIBRARIES(${LIB_NAME} ${M_LIB})
# zlib:
IF (ZLIB_FOUND)
    TARGET_INCLUDE_DIRECTORIES(${LIB_NAME} PUBLIC ${ZLIB_INCLUDE_DIR})
    TARGET_LINK_LIBRARIES(${LIB_NAME} ${ZLIB_LIBRARIES})
    # For cfitsio.pc.cmake:
    SET (PKG_CONFIG_LIBS "${PKG_CONFIG_LIBS} -lz")
ENDIF(ZLIB_FOUND)
# cURL:
IF (CURL_FOUND)
    TARGET_INCLUDE_DIRECTORIES(${LIB_NAME} PUBLIC ${CURL_INCLUDE_DIRS})
    TARGET_LINK_LIBRARIES(${LIB_NAME} ${CURL_LIBRARIES})
    # For cfitsio.pc.cmake:
    SET (PKG_CONFIG_LIBS "${PKG_CONFIG_LIBS} -lcurl")
ENDIF(CURL_FOUND)
# PThreads:
IF (THREADS_FOUND)
    TARGET_LINK_LIBRARIES(${LIB_NAME} Threads::Threads)
ENDIF(THREADS_FOUND)
# BZip2:
IF (BZIP2_FOUND)
    TARGET_INCLUDE_DIRECTORIES(${LIB_NAME} PUBLIC ${BZIP2_INCLUDE_DIRS})
    TARGET_LINK_LIBRARIES(${LIB_NAME} ${BZIP2_LIBRARIES})
    SET (PKG_CONFIG_LIBS "${PKG_CONFIG_LIBS} -lbz2")
ENDIF(BZIP2_FOUND)

SET_TARGET_PROPERTIES(${LIB_NAME} PROPERTIES VERSION ${${PROJECT_NAME}_VERSION} SOVERSION ${${PROJECT_NAME}_MAJOR})

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

install(TARGETS ${LIB_NAME} EXPORT ${LIB_NAME}-targets
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install(EXPORT ${LIB_NAME}-targets DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/cfitsio)
install(FILES ${H_FILES} DESTINATION ${INCLUDE_INSTALL_DIR} COMPONENT Devel)

#==============================================================================
# Optional test programs:
#==============================================================================
IF (TESTS)

    ENABLE_TESTING()

    ADD_EXECUTABLE(TestProg utilities/testprog.c)
    TARGET_LINK_LIBRARIES(TestProg ${LIB_NAME})
    ADD_TEST(TestProg TestProg)
    # Copy testprog.tpt to build directory to allow quick test
    # of ./TestProg (or .\Release\TestProg.exe in MSVC):
    FILE(COPY ${CMAKE_SOURCE_DIR}/testprog.tpt DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

    ADD_EXECUTABLE(cookbook utilities/cookbook.c)
    TARGET_LINK_LIBRARIES(cookbook ${LIB_NAME})
    ADD_TEST(cookbook cookbook)

ENDIF(TESTS)

#==============================================================================
# Optional utilities:
#==============================================================================
IF (UTILS)

    ADD_EXECUTABLE(FPack utilities/fpack.c utilities/fpackutil.c)
    TARGET_LINK_LIBRARIES(FPack ${LIB_NAME})

    ADD_EXECUTABLE(Funpack utilities/funpack.c utilities/fpackutil.c)
    TARGET_LINK_LIBRARIES(Funpack ${LIB_NAME})

    ADD_EXECUTABLE(Fitscopy utilities/fitscopy.c)
    TARGET_LINK_LIBRARIES(Fitscopy ${LIB_NAME})

    ADD_EXECUTABLE(Fitsverify
                  utilities/ftverify.c utilities/fvrf_data.c
                  utilities/fvrf_file.c utilities/fvrf_head.c
                  utilities/fvrf_key.c utilities/fvrf_misc.c)
    TARGET_COMPILE_DEFINITIONS(Fitsverify PRIVATE -DSTANDALONE)
    TARGET_LINK_LIBRARIES(Fitsverify ${LIB_NAME})

    # To expand the command line arguments in Windows, see:
    # http://msdn.microsoft.com/en-us/library/8bch7bkk.aspx
    if(MSVC AND NOT CMAKE_SYSTEM_NAME MATCHES "WindowsStore")
      set_target_properties(FPack Funpack PROPERTIES LINK_FLAGS "setargv.obj")
    endif(MSVC)

    install(TARGETS FPack Funpack Fitscopy Fitsverify RUNTIME DESTINATION bin)

ENDIF(UTILS)

#==============================================================================
# cfitsio.pc
#==============================================================================
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cfitsio.pc.cmake ${CMAKE_CURRENT_BINARY_DIR}/cfitsio.pc @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cfitsio-config.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/cfitsio-config.cmake @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cfitsio-config-version.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/cfitsio-config-version.cmake @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cfitsio.pc DESTINATION lib/pkgconfig/)
install(FILES ${CFITSIO_BINARY_DIR}/cfitsio-config.cmake
              ${CFITSIO_BINARY_DIR}/cfitsio-config-version.cmake
              DESTINATION lib/cfitsio-${CFITSIO_VERSION})
