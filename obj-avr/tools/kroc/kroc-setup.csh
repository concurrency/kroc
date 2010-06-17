if ( $?PATH ) then
	setenv PATH "/home/dennis/pkg/kroc-avr/bin:$PATH"
else
	setenv PATH "/home/dennis/pkg/kroc-avr/bin"
endif
if ( $?MANPATH ) then
	setenv MANPATH ":/home/dennis/pkg/kroc-avr/share/man:$MANPATH"
else
	setenv MANPATH ":/home/dennis/pkg/kroc-avr/share/man"
endif
if ( $?LD_LIBRARY_PATH ) then
	setenv LD_LIBRARY_PATH "/home/dennis/pkg/kroc-avr/lib:$LD_LIBRARY_PATH"
else
	setenv LD_LIBRARY_PATH "/home/dennis/pkg/kroc-avr/lib"
endif
if ( $?PKG_CONFIG_PATH ) then
	setenv PKG_CONFIG_PATH "/home/dennis/pkg/kroc-avr/lib/pkgconfig:$PKG_CONFIG_PATH"
else
	setenv PKG_CONFIG_PATH "/home/dennis/pkg/kroc-avr/lib/pkgconfig"
endif
if ( $?ACLOCAL ) then
	setenv ACLOCAL "$ACLOCAL -I /home/dennis/pkg/kroc-avr/share/aclocal"
else
	setenv ACLOCAL "aclocal -I /home/dennis/pkg/kroc-avr/share/aclocal"
endif
