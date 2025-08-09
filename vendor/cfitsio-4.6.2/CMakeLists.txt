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

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)

# Define project version
SET(${PROJECT_NAME}_SONAME 10)
SET(${PROJECT_NAME}_MAJOR 4)
SET(${PROJECT_NAME}_MINOR 6)
SET(${PROJECT_NAME}_MICRO 2)
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
INCLUDE(GNUInstallDirs)

#==============================================================================
# Build options:
#==============================================================================

# Allow the developer to select if Dynamic or Static libraries are built
OPTION(BUILD_SHARED_LIBS "Build Shared Libraries" ON)

# Allow curl dependency to be disabled with "-DUSE_CURL=OFF":
OPTION(USE_CURL "USE_CURL" ON)

OPTION(USE_PTHREADS "Thread-safe build (using pthreads)" OFF)

OPTION(TESTS "Build test programs Testprog and cookbook" ON)

OPTION(UTILS "Build helper programs [fpack, funpack, fitscopy, fitsverify, imcopy, smem, speed]" ON)

OPTION(USE_SSE2 "Enable use of instructions in the SSE2 extended instruction set" OFF)

OPTION(USE_SSSE3 "Enable use of instructions in the SSSE3 extended instruction set" OFF)

OPTION(USE_BZIP2 "Enable bzip2 support" OFF)

#==============================================================================

FIND_PACKAGE(ZLIB REQUIRED)

IF(BUILD_SHARED_LIBS)
  SET (LIB_TYPE SHARED)
ELSE()
  SET (LIB_TYPE STATIC)
ENDIF(BUILD_SHARED_LIBS)

# Find curl library, for HTTPS support:
IF(USE_CURL)
   FIND_PACKAGE(CURL)
   IF(CURL_FOUND)
      ADD_DEFINITIONS(-DCFITSIO_HAVE_CURL)
   ENDIF()
ENDIF()

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

include(GenerateExportHeader)

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

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}")

# For future modifications:

# drvrsmem.c is only usable if HAVE_SHMEM_SERVICES is defined:
#drvrsmem.c

# drvrgsiftp.c is only usable if HAVE_NET_SERVICES & HAVE_GSIFTP are defined:
#drvrgsiftp.c

#==============================================================================
# Add libraries:
#==============================================================================

ADD_LIBRARY(${LIB_NAME} ${LIB_TYPE} ${H_FILES} ${SRC_FILES})

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

#==============================================================================

generate_export_header(cfitsio)
set_property(TARGET cfitsio PROPERTY VERSION ${CFITSIO_VERSION})
set_property(TARGET cfitsio PROPERTY SOVERSION 10)
set_property(TARGET cfitsio PROPERTY INTERFACE_cfitsio_MAJOR_VERSION ${${PROJECT_NAME}_MAJOR})
set_property(TARGET cfitsio APPEND PROPERTY
  COMPATIBLE_INTERFACE_STRING cfitsio_MAJOR_VERSION
)

install(TARGETS cfitsio EXPORT cfitsioTargets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(FILES ${H_FILES} "${CMAKE_CURRENT_BINARY_DIR}/cfitsio_export.h"
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT Devel)

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/cfitsio/cfitsioConfigVersion.cmake"
  VERSION ${CFITSIO_VERSION}
  COMPATIBILITY AnyNewerVersion
)

export(EXPORT cfitsioTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/cfitsio/cfitsioTargets.cmake"
  NAMESPACE CFITSIO::
)
configure_file(cmake/cfitsioConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/cfitsio/cfitsioConfig.cmake"
  COPYONLY
)

set(ConfigPackageLocation ${CMAKE_INSTALL_LIBDIR}/cmake/cfitsio)
install(EXPORT cfitsioTargets
  FILE
    cfitsioTargets.cmake
  NAMESPACE
    CFITSIO::
  DESTINATION
    ${ConfigPackageLocation}
)
install(
  FILES
    cmake/cfitsioConfig.cmake
    "${CMAKE_CURRENT_BINARY_DIR}/cfitsio/cfitsioConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
  COMPONENT
    Devel
)

# cfitsio.pc:
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cfitsio.pc.cmake ${CMAKE_CURRENT_BINARY_DIR}/cfitsio.pc @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cfitsio.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)

#==============================================================================
# Test programs:
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

    install(TARGETS cookbook RUNTIME DESTINATION bin)

ENDIF(TESTS)

#==============================================================================
# Utilities:
#==============================================================================
IF (UTILS)

    ADD_EXECUTABLE(fpack utilities/fpack.c utilities/fpackutil.c)
    TARGET_LINK_LIBRARIES(fpack ${LIB_NAME})

    ADD_EXECUTABLE(funpack utilities/funpack.c utilities/fpackutil.c)
    TARGET_LINK_LIBRARIES(funpack ${LIB_NAME})

    ADD_EXECUTABLE(fitscopy utilities/fitscopy.c)
    TARGET_LINK_LIBRARIES(fitscopy ${LIB_NAME})

    ADD_EXECUTABLE(fitsverify
                  utilities/ftverify.c utilities/fvrf_data.c
                  utilities/fvrf_file.c utilities/fvrf_head.c
                  utilities/fvrf_key.c utilities/fvrf_misc.c)
    TARGET_COMPILE_DEFINITIONS(fitsverify PRIVATE -DSTANDALONE)
    TARGET_LINK_LIBRARIES(fitsverify ${LIB_NAME})

    ADD_EXECUTABLE(imcopy utilities/imcopy.c)
    TARGET_LINK_LIBRARIES(imcopy ${LIB_NAME})

    IF(HAVE_SHMEM_SERVICES)
      ADD_EXECUTABLE(smem utilities/smem.c)
      TARGET_LINK_LIBRARIES(smem ${LIB_NAME})
    ENDIF()

    ADD_EXECUTABLE(speed utilities/speed.c)
    TARGET_LINK_LIBRARIES(speed ${LIB_NAME})

    # To expand the command line arguments in Windows, see:
    # http://msdn.microsoft.com/en-us/library/8bch7bkk.aspx
    if(MSVC AND NOT CMAKE_SYSTEM_NAME MATCHES "WindowsStore")
      set_target_properties(fpack funpack PROPERTIES LINK_FLAGS "setargv.obj")
    endif()

    install(TARGETS fpack funpack fitscopy fitsverify imcopy speed RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
    IF(HAVE_SHMEM_SERVICES)
      install(TARGETS smem RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
    ENDIF()

ENDIF(UTILS)
