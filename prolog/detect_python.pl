#!/usr/bin/env swipl -q -g main -s
%
%  detect_python.pl
%  code
%
%  Created by Lars Yencken on 2011-09-29.
%  Coplright 2011 Lars Yencken. All rights reserved.
%

main :- 
    current_prolog_flag(argv, Argv),
    append(_, [--|_Av], Argv), !,
    main(Argv).

main(_Argv) :-
    (
        python_command(Command),
        which(Command, Path),
        print(Path),
        fail
    ;
        halt
    ).

python_command('python').
python_command('python2.7').
python_command('python2.6').
python_command('python2.5').
python_command('python2.4').
python_command('python3.2').
python_command('python3.1').
python_command('python3.0').

which(Command, Path) :-
    tmp_file(syscmd, TmpFile),
    atomic_list_concat(['which ', Command, ' > ', TmpFile], WhichCall),
    shell(WhichCall),
    read_file_to_codes(TmpFile, Codes, []),
    atom_codes(Path, Codes).
