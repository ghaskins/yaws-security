%define rpmrel _RPM_RELEASE

BuildRequires: erlang yaws eopenid

Summary: yaws_security: A spring-security inspired framework for Erlang/YAWS
Name: yaws_security
Version: _RPM_VERSION
License: GPL
Release: %{rpmrel}
Requires: erlang yaws eopenid
Group: Languages/Security
Source: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-root

%description
Authors
--------------------------
  Gregory Haskins <ghaskins@novell.com>

%debug_package
%prep
%setup

%build
make

%install
make install INSTPATH=$RPM_BUILD_ROOT%{_libdir}/erlang/lib

# Install documentation  
%clean
make clean

%files
%defattr(-,root,root)
%{_libdir}/erlang/lib/%{name}-%{version}.%{release}

%changelog
