-module(test).

-export([f/1,
         f_records/1,
         f_maps/1]).


-type t() :: a | b.

-spec f(t()) -> ok | not_ok.
f(T) ->
    case T of
        a -> ok
        %b -> not_ok
    end.


-record(r1, {a = 1}).
-type r1() :: #r1{}.

-record(r2, {b = 2}).
-type r2() :: #r2{}.

-type record_variants() :: r1() | r2().

-spec f_records(record_variants()) -> ok | not_ok.
f_records(R) ->
    case R of
        #r1{} -> ok
        %#r2{} -> ok
    end.


-type m1() :: #{a := 1}.
-type m2() :: #{b := 2}.

-type map_variants() :: m1() | m2().

-spec f_maps(map_variants()) -> ok | not_ok.
f_maps(M) ->
    case M of
        #{a := _} -> ok
        %#{b := _} -> ok
    end.
