#!/usr/bin/env swipl -q -g main -s
%
%  hello.pl
%  code
%
%  Created by Lars Yencken on 2011-09-29.
%  Coplright 2011 99designs. All rights reserved.
%

main :- 
    current_prolog_flag(argv, Argv),
    append(_, [--|_Av], Argv), !,
    main(Argv).

main(_Argv) :-
    print('Hello world!\n'),
    halt.
