
PROC="salloc"
ARG=1

L="math pkg sys/clio sys/dbio sys/etc sys/fio sys/fmio sys/fmtio sys/gio sys/gty sys/imfort sys/imio sys/ki sys/mtio sys/mwcs sys/plio sys/pmio sys/psio sys/qpoe sys/symtab sys/tty"

for i in $L ; do
  LIST=`find $i | grep -v '\.svn' | grep '\.[g]*x$'`
  if [ "$LIST" != "" ]; then
    ./_secret_weapons/add_sz_val $PROC $ARG $LIST
    S=$?
    if [ $S != 0 ]; then
	break
    fi
  fi
done
