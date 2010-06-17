#!/bin/bash

# Cross-Compiler Toolchain for ${PLATFORM}
#  by Martin Decky <martin@decky.cz>
#  uploaded to http://stackoverflow.com/questions/1245295/building-arm-gnu-cross-compiler
#  on Aug 7 '09.
#
#  Modified by Drew Pirrone-Brusse
#  on June 3, '10
#
#  GPL'ed, copyleft
#


check_error() {
    if [ "$1" -ne "0" ]; then
        echo
        echo "Script failed: $2"
        exit
    fi
}



#if [ -z "${CROSS_PREFIX}" ] ; then
    CROSS_PREFIX="/usr/local-sfp"
#fi

BINUTILS_VERSION="2.20.1"
GCC_VERSION="4.4.4"
NEWLIB_VERSION="1.18.0"

BINUTILS="binutils-${BINUTILS_VERSION}.tar.gz"
NEWLIB="newlib-${NEWLIB_VERSION}.tar.gz"
GCC_CORE="gcc-core-${GCC_VERSION}.tar.bz2"

BINUTILS_SOURCE="ftp://ftp.gnu.org/gnu/binutils/"
GCC_SOURCE="ftp://ftp.gnu.org/gnu/gcc/gcc-${GCC_VERSION}/"
NEWLIB_SOURCE="ftp://sources.redhat.com/pub/newlib/"

TARGET="arm-elf"
PREFIX="${CROSS_PREFIX}/${TARGET}" #/usr/local/arm-elf
WORKDIR=`pwd`
SRCDIR="${WORKDIR}/gcc-${GCC_VERSION}-SRC"
BUILDDIR="${WORKDIR}/gcc-${TARGET}"
BINUTILSDIR="${SRCDIR}/binutils-${BINUTILS_VERSION}"
GCCDIR="${SRCDIR}/gcc-${GCC_VERSION}"
NEWLIBDIR="${SRCDIR}/newlib-${NEWLIB_VERSION}"

echo ">>> Creating destionation directory"
if [ ! -d "${PREFIX}" ]; then
        ################# SUDO GOES HERE!!
        echo ">>>>>>>>>> We need sudo access to create a folder in ${PREFIX}."
    sudo mkdir -p "${PREFIX}"
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
    ################# SUDO GOES HERE!!
    echo ">>>>>>>>>> We need sudo access to install binutils."
sudo make install
check_error $? "Error installing binutils."

echo ">>> Compiling and installing GCC first pass"
cd "${BUILDDIR}"
check_error $? "Change directory failed."
"${GCCDIR}/configure" "--target=${TARGET}" "--prefix=${PREFIX}" "--enable-interwork" "--enable-multilib" "--enable-languages=c" "--with-newlib" "--with-headers=${NEWLIBDIR}/newlib/libc/include" "--disable-werror" "--with-float=soft"
check_error $? "Error configuring GCC."
PATH="${PATH}:${PREFIX}/bin" make all-gcc
check_error $? "Error making GCC first pass."
    ################# SUDO GOES HERE!!
    echo ">>>>>>>>>> We need sudo access to install the first pass of GCC."
sudo PATH="${PATH}:${PREFIX}/bin" make install-gcc
check_error $? "Error installing GCC first pass."

echo ">>> Linking arm-elf-gcc to arm-elf-cc."
    ################# SUDO GOES HERE!!
    echo ">>>>>>>>>> We need sudo access to link files in usr/..."
sudo ln -s "${PREFIX}/bin/arm-elf-gcc" "${PREFIX}/bin/arm-elf-cc"
check_error $? "Error linking arm-elf-gcc to arm-elf-cc."

echo ">>> Compiling and installing newlib."
cd "${NEWLIBDIR}"
check_error $? "Change directory failed."
./configure "--target=${TARGET}" "--prefix=${PREFIX}" "--enable-interwork" "--enable-multilib" "--with-float=soft"
check_error $? "Error configuring newlib."
PATH="${PATH}:${PREFIX}/bin" make 
check_error $? "Error making newlib."
    ################# SUDO GOES HERE!!
    echo ">>>>>>>>>> We need sudo access to install newlib."
sudo PATH="${PATH}:${PREFIX}/bin" make install
check_error $? "Error installing newlib."


echo ">>> Compiling and installing GCC second pass"
cd "${BUILDDIR}"
PATH="${PATH}:${PREFIX}/bin" make all
check_error $? "Error making GCC second pass"
    ################# SUDO GOES HERE!!
    echo ">>>>>>>>>> We need sudo access to install the second pass of GCC."
sudo PATH="${PATH}:${PREFIX}/bin" make install
check_error $? "Error installing GCC second pass."

echo
echo ">>> Cross-compiler for ${TARGET} installed."

