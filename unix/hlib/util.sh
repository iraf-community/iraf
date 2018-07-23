#!/bin/sh
#
#  UTIL.SH -- Utility support script for IRAF commands.
#
# ----------------------------------------------------------------------------


##############################################################################
#  Start of MACHDEP definitions.
##############################################################################

# MACHDEP definitions which may be reset below.
LS() {
    ls 					# [MACHDEP]
} ; export LS


export IRAF_VERSION="V2.16"
export V=$(printf "%s" $IRAF_VERSION | cut -c2-5)

export hilite=1


##############################################################################
#  Utility aliases.
##############################################################################

ECHO() {
    if [ "$1" = "-n" ]; then
        printf "%s" "$2"
    else
        printf "%s\\n" "$1"
    fi
} ; export ECHO

RM() {
    rm -rf "$1"
} ; export RM

LN() {
    ln -s "$2" "$1"
} ; export LN

BOLD_ON() {
    if [ $hilite ]; then tput bold; fi
} ; export BOLD_ON
BOLD_OFF() {
    if [ $hilite ]; then tput sgr0; fi
} ; export BOLD_OFF
SO_ON() {
    if [ $hilite ]; then tput smso; fi
} ; export SO_ON
SO_OFF() {
    if [ $hilite ]; then tput sgr0; fi
} ; export SO_OFF

DO_OK() {
    ECHO -n "[ "; BOLD_ON; ECHO -n " OK "; BOLD_OFF; ECHO " ]"
} ; export DO_OK

DO_WARN() {
    ECHO -n "[ "; BOLD_ON; ECHO -n "WARN"; BOLD_OFF; ECHO " ]"
} ; export DO_WARN

DO_FAIL() {
    ECHO -n "[ ";   SO_ON; ECHO -n "FAIL";   SO_OFF; ECHO " ]"
} ; export DO_FAIL

NEWLINE() {
    ECHO ''
} ; export NEWLINE



PUT() {
    cp -p "$1" "$2"
} ; export PUT

PROMPT() {
    BOLD_ON; ECHO -n "$1"; BOLD_OFF; ECHO -n " (yes): "
} ; export PROMPT

PROMPT_N() {
    BOLD_ON; ECHO -n "$1"; BOLD_OFF; ECHO -n " (no): "
} ; export PROMPT_N

MSG() {
    ECHO -n "   "; BOLD_ON; ECHO -n "***  "; BOLD_OFF; ECHO "$1"
} ; export MSG

MSGB() {
    ECHO -n "   ";BOLD_ON;ECHO -n "***  ";ECHO "$1"; BOLD_OFF
} ; export MSGB

MSGN() {
    ECHO -n "   ";BOLD_ON;ECHO -n "***  ";BOLD_OFF; ECHO -n "$1"
} ; export MSGN

MSGBN() {
    ECHO -n "   ";BOLD_ON;ECHO -n "***  ";ECHO -n "$1"; BOLD_OFF
} ; export MSGBN

ERRMSG() {
    ECHO -n "   ";BOLD_ON;ECHO -n "ERROR: "  ;BOLD_OFF; ECHO "$1"
} ; export ERRMSG

WARNING() {
    ECHO -n "   ";
    BOLD_ON;
    ECHO -n "WARNING: ";
    BOLD_OFF;
    ECHO "$1"
} ; export WARNING


