#display /i $pc

set height 0

define a
step
end

define ps
call spp_printstr ($arg0)
end

define pc
call spp_printmemc ($arg0)
end


dir /local/src/tcl/tcl7.5/unix
dir /local/src/tcl/tcl7.5/compat
dir /local/src/tcl/tcl7.5/generic

dir /iraf/iraf/unix/os
dir /iraf/iraf/unix/boot/bootlib
dir /iraf/iraf/unix/boot/spp
dir /iraf/iraf/unix/boot/rtar
dir /iraf/iraf/unix/boot/mkpkg

dir /iraf/iraf/sys/fio
dir /iraf/iraf/sys/fmio
dir /iraf/iraf/sys/fmtio
dir /iraf/iraf/sys/clio
dir /iraf/iraf/sys/etc/gen
dir /iraf/iraf/sys/etc
dir /iraf/iraf/sys/imio
dir /iraf/iraf/sys/imio/iki
dir /iraf/iraf/sys/imio/iki/fxf
dir /iraf/iraf/sys/imio/iki/oif
dir /iraf/iraf/sys/imio/iki/stf
dir /iraf/iraf/sys/gty
dir /iraf/iraf/sys/memio
dir /iraf/iraf/sys/mtio
dir /iraf/iraf/sys/symtab
dir /iraf/iraf/sys/tty
dir /iraf/iraf/sys/ki

dir /iraf/iraf/sys/gio
dir /iraf/iraf/sys/gio/gki
dir /iraf/iraf/sys/gio/imdkern
dir /iraf/iraf/sys/gio/cursor

dir /iraf/iraf/sys/imfort

dir /iraf/iraf/pkg/system
dir /iraf/iraf/pkg/images/imutil/src

dir .
