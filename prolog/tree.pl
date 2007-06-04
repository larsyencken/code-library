%-----------------------------------------------------------------------------%
% tree.pl
% Lars Yencken <lljy@csse.unimelb.edu.au>
% vim: ts=4 sw=4 et tw=78:
% Tue Mar 28 13:23:51 EST 2006
%
%-----------------------------------------------------------------------------%

:- module(tree, []).

%-----------------------------------------------------------------------------%

% init(+Tree).
init(empty).

%-----------------------------------------------------------------------------%

% insert(+Tree0, +Element, -Tree).
insert(empty, Element, Tree) :- Tree = tree(Element, empty, empty).
insert(tree(Value, Left0, Right0), Element, tree(Value, Left, Right)) :-
    ( compare(<, Element, Value) ->
        insert(Left0, Element, Left),
        Right = Right0
    ;
        insert(Right0, Element, Right),
        Left = Left0
    ).

%-----------------------------------------------------------------------------%

% contains(+Tree, ?Value) is nondet.
%   Looks for the given value in the tree. If Value is left unbound, it will
%   iterate over all the values in the tree, in MLR order.
contains(tree(Value, _Left, _Right), Value).
contains(tree(_Value, Left, Right)) :-
    (
        contains(Left, Value)
    ;
        contains(Right, Value)
    ).

%-----------------------------------------------------------------------------%

% fromList(+List, -Tree) is det.
%   Constructs a tree from an unsorted list.
fromList(List, Tree) :-
    reduce(insert, [empty|List], Tree).

%-----------------------------------------------------------------------------%

% toList(+Tree, -List) is det.
%   Flattens the tree into a sorted list.
toList(empty, []).
toList(tree(Value, Left, Right), List) :-
    toList(Left, ListLeft),
    toList(Right, ListRight),
    append(ListLeft, [Value|ListRight], List).

%-----------------------------------------------------------------------------%

% sortList(+List0, -List) is det.
%   Sorts a list by constructing a tree out of it, then flattening the
%   resulting tree.
sortList(List0, List) :-
    fromList(List0, Tree),
    toList(Tree, List).

%-----------------------------------------------------------------------------%

% mergeTree(+TreeA, +TreeB, -Tree) is det.
%   Merges two trees together, creating a third.
mergeTree(TreeA, TreeB, Tree) :-
    toList(TreeB, List),
    reduce(insert, [TreeA|List], Tree).

%-----------------------------------------------------------------------------%

% delete(+Tree0, +Value, -Tree) is semidet.
delete(tree(NodeValue, Left0, Right0), Value, Tree) :-
    ( NodeValue = Value ->
        % delete from here, result will be the lower trees.
        mergeTree(Left0, Right0, Tree)
    ; compare(<, Value, NodeValue) ->
        % follow the left branch
        Tree = tree(NodeValue, Left, Right0),
        delete(Left0, Value, Left)
    ;
        % follow the right branch
        Tree = tree(NodeValue, Left0, Right),
        delete(Right0, Value, Right)
    ).

%-----------------------------------------------------------------------------%

% reduce(+Predicate, +List, -Value) is semidet.
%   Applies the predicate to the first two elements in the list, reducing them
%   to a single element. Then continues in this way with the new element,
%   eventually reducing the entire list to a single value.
reduce(Predicate, List, Value) :-
    (
        List = [Value], !
    ;
        List = [ElemA, ElemB|Rest],
        call(Predicate, ElemA, ElemB, Result),
        reduce(Predicate, [Result|Rest], Value)
    ).

%-----------------------------------------------------------------------------%

