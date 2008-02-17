#!/bin/sh

do_update() {

  L="math pkg sys/clio sys/dbio sys/etc sys/fio sys/fmio sys/fmtio sys/gio sys/gty sys/imfort sys/imio sys/ki sys/mtio sys/mwcs sys/plio sys/pmio sys/psio sys/qpoe sys/symtab sys/tty"

  for i in $L ; do
    LIST=`find $i | grep -v -e '\.svn' -e 'evvexpr\.' -e 'evexpr\.' | grep '\.[g]*x$'`
    if [ "$LIST" != "" ]; then
      ./_secret_weapons/add_sz_val $1 $2 $3 $4 $LIST
      S=$?
      if [ $S != 0 ]; then
  	exit $S
      fi
    fi
  done

}

PROC_LIST="salloc:1 malloc:1 realloc:1 calloc:1 amovX:2 amovkX:2 aclrX:1 alimX:1 bswap2:4,1,3 bswap4:4,1,3 bswap8:4,1,3 aabsX:2 aaddX:3 asubX:3 awsuX:3 adivkX:3 amulkX:3 amapX:2 asrtX:2 aaddkX:3 amulX:3 ahgmX:1,3 asubkX:3 asqrX:2 altmX:2 altaX:2 abavX:3 ablekX:3 abgekX:3 absuX:3 apowkX:3 apkxX:3 aandkX:3 aminX:3 aminkX:3 aavgX:1 anegX:2 adivX:3 abeqkX:3 arcpX:3 arczX:3 argtX:1 amaxX:3 amaxkX:3 aluiX:3 alutX:2 allnX:2 alogX:2 amedX:1 amed3X:4 amed5X:6 advzX:3 abnekX:3 axorkX:3 aupxX:3 bytmov:4,1,3 miipksize:0 miinelem:0 miiupk32:2 miipak:2 miipak32:2 miiupk:2 miiupk16:2 miiupk8:2 miiupkr:2 miipak16:2 miilen:0 miiupkd:2 miipak8:2 miipakr:2 miipakd:2 achtbX:2 achtcX:2 achtdX:2 achtiX:2 achtlX:2 achtpX:2 achtrX:2 achtsX:2 achtxX:2"

# acht..

TYPE="size_t"
BASE_VALNAME="sz_val"

for j in $PROC_LIST ; do
  echo "# Updating ... $j"
  PROCS=`echo $j | sed -e 's/[X]*:.*//'`
  ARGNO_LIST=`echo $j | sed -e 's/.*://' | tr ',' ' '`
  CNT=0
  for k in $ARGNO_LIST ; do
    if [ $CNT = 0 ]; then
      VALNAME=$BASE_VALNAME
    else
      VALNAME=${BASE_VALNAME}$CNT
    fi
    if [ "`echo $j|grep 'X'`" != "" ]; then
      for i in b c d i l p r s x '$t' ; do
        do_update ${PROCS}$i $k $TYPE $VALNAME
      done
    else
      do_update $PROCS $k $TYPE $VALNAME
    fi
    CNT=`expr $CNT + 1`
  done
  #echo $PROCS
  #echo $ARGNO
done

