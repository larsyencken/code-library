%------------------------------------------------------------------------------%
% hello.m
% Lars Yencken <lars.yencken@gmail.com>
% vim: ts=4 sw=4 et tw=78:
% Sat Nov 17 01:49:03 CET 2007
%
%------------------------------------------------------------------------------%

:- module hello.


%------------------------------------------------------------------------------%

:- interface.

:- import_module io.

:- pred main(io.io, io.io).
:- mode main(di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

main --> io.write_string("Hello World!\n").

%------------------------------------------------------------------------------%

