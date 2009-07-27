Name:           kroc
Version:        1.5.0
Release:        1%{?dist}
Summary:        The occam-pi toolchain for parallel programming on Linux and the Arduino
Group:          Development/Tools
License:        GPLv2+ 
URL:            http://www.concurrency.cc/ 
Source0:        http://www.concurrency.cc/dist/kroc-1.5.0.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
#BuildRequires:  
#Requires:       

%description
Occam-pi is a fundamentally parallel programming language based on the 
CSP and Pi-calculi. This package provides the toolchain for use on the 
command-line; support for compiling and running programs under Linux and
on the AVR family of processors (specifically, on the open-hardware 
platform known on the Arduino) is also provided.

%prep
%setup -q

%build
autoreconf -vfi
./configure \
  --with-toolchain=tvm \
  --prefix=/usr \
  --libdir=${PREFIX}/lib
make %{?_smp_mflags}

%install
rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%doc README BUGS CHANGELOG
%{_bindir}/*
%{_datadir}/tvm
%{_datadir}/kroc
%{_mandir}/*/*

%exclude %{_datadir}/aclocal/*
%exclude %{_includedir}/*
%exclude %{_libdir}/*
%exclude %{_bindir}/kroc-setup.sh
%exclude %{_bindir}/kroc-setup.csh

%changelog
* Wed Jul 26 2009 Matthew Jadud <matt@concurrency.cc> 1.5.0-1.fc11 
- First packageing of toolchain for desktop use.
