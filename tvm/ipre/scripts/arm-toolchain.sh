#!/bin/bash

#  This script will automate the process of downloading, configuring, making
#  and installing a functioning GCC toolchain.
#  It was only tested in the context of the ARM-ELF-GCC toolchain, so there
#  are no guarantees that it works for any other target. (There are, in fact, 
#   no guarantees it works for the arm-elf, even, but that's halfway-hacked
#   open-source for you. ;)
#
#  It requires the following packages to be pre-installed
#  (RPM)	gmp-devel mpfr-devel libmpc-devel texinfo wget make 
#  (Debian)	libgmpe-dev libmpfr-dev libmpc-dev texinfo wget make
#
#  First written by Martin Decky <martin@decky.cz>
#  uploaded to http://stackoverflow.com/questions/1245295/building-arm-gnu-cross-compiler
#  on Aug 7 '09.
#  Modified by Drew Pirrone-Brusse
#  on June 3, '10
#
#  Made available under the GNU General Public Liscense
#		http://www.fsf.org/licensing/

TARGET="arm-elf"
CROSS_PREFIX="/usr/local"

PREFIX="${CROSS_PREFIX}/${TARGET}" #/usr/local/arm-elf

BINUTILS_VERSION="2.20.1"
GCC_VERSION="4.4.4"
NEWLIB_VERSION="1.18.0"

#  Uncomment this to have the script require you to enter any sudo command
#BABYSIT=1

run_usr_cmd() {
    echo
    echo
    echo ">>> We need sudo access to run the following command:"
    echo ">>>          $@"
    echo ">>>  Please enter the command to continue."
    read CMD
    test "$@" == "${CMD}" && `${CMD}` || (
      echo "The command you entered doesn't match up to what we expect."
      read -p "Enter 0 to enter the string again, or 1 to run the command you entered: " REDO
      test "${REDO}" == "1" && `${CMD}`|| run_usr_cmd $@
    )
}

check_error() {
    if [ "$1" -ne "0" ]; then
        echo
        echo "Script failed: $2"
        exit
    fi
}

BINUTILS="binutils-${BINUTILS_VERSION}.tar.gz"
NEWLIB="newlib-${NEWLIB_VERSION}.tar.gz"
GCC_CORE="gcc-core-${GCC_VERSION}.tar.bz2"

BINUTILS_SOURCE="ftp://ftp.gnu.org/gnu/binutils/"
GCC_SOURCE="ftp://ftp.gnu.org/gnu/gcc/gcc-${GCC_VERSION}/"
NEWLIB_SOURCE="ftp://sources.redhat.com/pub/newlib/"

WORKDIR=`pwd`
SRCDIR="${WORKDIR}/gcc-${GCC_VERSION}-SRC"
BUILDDIR="${WORKDIR}/gcc-${TARGET}"
BINUTILSDIR="${SRCDIR}/binutils-${BINUTILS_VERSION}"
GCCDIR="${SRCDIR}/gcc-${GCC_VERSION}"
NEWLIBDIR="${SRCDIR}/newlib-${NEWLIB_VERSION}"

#BREAK=0

echo ">>> Creating destionation directory"
if [ ! -d "${PREFIX}" ]; then
    if [ ${BABYSIT} ]; then
	run_usr_cmd "sudo mkdir -p ${PREFIX}"
    else
	sudo mkdir -p ${PREFIX}
    fi
    test -d "${PREFIX}"
    check_error $? "Unable to create ${PREFIX}."
fi

echo ">>> Creating GCC build directory"
if [ ! -d "${BUILDDIR}" ]; then
    mkdir -p "${BUILDDIR}"
    test -d "${BUILDDIR}"
    check_error $? "Unable to create ${BUILDDIR}."
fi

echo ">>> Creating GCC source directory"
if [ ! -d "${SRCDIR}" ]; then
    mkdir -p "${SRCDIR}"
    test -d "${SRCDIR}"
    check_error $? "Unable to create ${SRCDIR}."
fi


echo ">>> Downloading tarballs"

if [ ! -f "${SRCDIR}/${BINUTILS}" ]; then
    wget -c --directory-prefix="$SRCDIR" "${BINUTILS_SOURCE}${BINUTILS}"
    check_error $? "Error downloading binutils."
fi
if [ ! -f "${SRCDIR}/${NEWLIB}" ]; then
    wget -c --directory-prefix="$SRCDIR" "${NEWLIB_SOURCE}${NEWLIB}"
    check_error $? "Error downloading newlib."
fi
if [ ! -f "${SRCDIR}/${GCC_CORE}" ]; then
    wget -c --directory-prefix="$SRCDIR" "${GCC_SOURCE}${GCC_CORE}"
    check_error $? "Error downloading GCC Core."
fi

echo ">>> Unpacking tarballs"
tar -xvzf "${SRCDIR}/${BINUTILS}" -C ${SRCDIR}
check_error $? "Error unpacking binutils."
tar -xvzf "${SRCDIR}/${NEWLIB}" -C ${SRCDIR}
check_error $? "Error unpacking newlib."
tar -xvjf "${SRCDIR}/${GCC_CORE}" -C ${SRCDIR}
check_error $? "Error unpacking GCC Core."


echo ">>> Compiling and installing binutils"
cd "${BINUTILSDIR}"
check_error $? "Change directory failed."
./configure "--target=${TARGET}" "--prefix=${PREFIX}" "--enable-interwork" "--enable-multilib" "--disable-werror"
check_error $? "Error configuring binutils."
make all
check_error $? "Error making binutils."
echo ">>> Installing binutils."
if [ $BABYSIT]; then
    run_usr_cmd "sudo make install"
else
    sudo make install
fi
check_error $? "Error installing binutils."

PATH="${PATH}:${PREFIX}/bin"

echo ">>> Compiling and installing GCC first pass"
cd "${BUILDDIR}"
check_error $? "Change directory failed."
"${GCCDIR}/configure" "--target=${TARGET}" "--prefix=${PREFIX}" "--enable-interwork" "--enable-multilib" "--enable-languages=c" "--with-newlib" "--with-headers=${NEWLIBDIR}/newlib/libc/include" "--disable-werror"
check_error $? "Error configuring GCC."
make all-gcc
check_error $? "Error making GCC first pass."
echo ">>> Installing GCC."
if [ ${BABYSIT} ]; then
    run_usr_cmd "sudo make install-gcc"
else
    sudo make install-gcc
fi
check_error $? "Error installing GCC first pass."

if [ "${TARGET}" == "arm-elf" ]
    echo ">>> Linking arm-elf-gcc"
    cd "${PREFIX}/bin"
    if [ ${BABYSIT} ]; then
        run_usr_cmd "ln -s arm-elf-gcc arm-elf-cc"
    else
        ln -s arm-elf-gcc arm-elf-cc
    fi
    check_error $? "Error linking arm-elf-gcc to arm-elf-cc."
fi

echo ">>> Compiling and installing newlib."
cd "${NEWLIBDIR}"
check_error $? "Change directory failed."
./configure "--target=${TARGET}" "--prefix=${PREFIX}" "--enable-interwork" "--enable-multilib"
check_error $? "Error configuring newlib."
make 
check_error $? "Error making newlib."
echo ">>> Installing newlib."
if [ ${BABYSIT} ]; then
    run_usr_cmd "sudo make install"
else
    sudo make install
fi
check_error $? "Error installing newlib."


echo ">>> Compiling and installing GCC second pass"
cd "${BUILDDIR}"
make all
check_error $? "Error making GCC second pass"
echo ">>> Installing GCC (again)."
if [ ${BABYSIT} ]; then
    run_usr_cmd "sudo make install"
else
    sudo make install
fi
check_error $? "Error installing GCC second pass."

echo
echo
echo ">>> Cross-compiler for ${TARGET} installed."
