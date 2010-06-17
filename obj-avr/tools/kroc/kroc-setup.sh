PATH="/home/dennis/pkg/kroc-avr/bin:$PATH"
if [ "x$LD_LIBRARY_PATH" = "x" ] ; then
	LD_LIBRARY_PATH="/home/dennis/pkg/kroc-avr/lib"
else
	LD_LIBRARY_PATH="/home/dennis/pkg/kroc-avr/lib:$LD_LIBRARY_PATH"
fi
export LD_LIBRARY_PATH
PKG_CONFIG_PATH="/home/dennis/pkg/kroc-avr/lib/pkgconfig:$PKG_CONFIG_PATH"; export PKG_CONFIG_PATH
MANPATH=":/home/dennis/pkg/kroc-avr/share/man:$MANPATH"; export MANPATH
ACLOCAL="${ACLOCAL:-aclocal} -I /home/dennis/pkg/kroc-avr/share/aclocal"; export ACLOCAL

