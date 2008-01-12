#{ KPNO.CL -- KPNO observing utilities package.

package	kpno

task	kpnofocus = "obsnoao$kpno/kpnofocus.cl"

if (access ("spectimedb$")) {
    set sptimeKPNO = "spectimedb$KPNO/"
    task mars = "spectimedb$scripts/mars.cl"
} else
    ;

clbye
