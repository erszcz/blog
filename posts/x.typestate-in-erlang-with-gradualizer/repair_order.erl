-module(repair_order).

-export([validate/1,
         invalid_use/0]).

-type result(Ok, Error) :: {ok, Ok} | {error, Error}.

-record(?MODULE, {order_number,
                  damage_description,
                  vehicle,
                  customer,
                  state}).
-type t() :: #?MODULE{state :: state()}.

-type invalid() :: {invalid, [string()]}.
-record(in_progress, {assigned_technician,
                      steps_left = []}).
-type in_progress() :: #in_progress{}.

-type state() :: new | valid | invalid() | in_progress()
               | work_done | waiting_for_payment | paid.

%-type new() :: new.
%-type valid() :: valid.
%-type invalid() :: {invalid, [string()]}.
%-record(in_progress, {assigned_technician,
%                      steps_left = []}).
%-type in_progress() :: #in_progress{}.
%-type work_done() :: work_done.
%-type waiting_for_payment() :: waiting_for_payment.
%-type paid() :: {paid, non_neg_integer()}.

-spec validate(t()) -> result(t(), t()).
validate(Order) ->
    case is_valid(Order) of
        true -> {ok, #?MODULE{state = valid}};
        %% This is correctly flagged as a type error:
        %% repair_order.erl: The tuple on line 29 at column 18 is expected to have type result(t(valid()), t(invalid())) but it has type {error, #repair_order{}}
        %%
        %%  case is_valid(Order) of
        %%      true -> {ok, #?MODULE{state = valid}};
        %%      false -> {error, #?MODULE{state = invalid}}
        %%               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        false -> {error, #?MODULE{state = invalid}}
        %false -> {error, #?MODULE{state = {invalid, ["error1", "error2"]}}}
    end.

-spec invalid_use() -> result(t(), t()).
invalid_use() ->
    %% this `asd' should be flagged as an error!
    validate(#?MODULE{state = asd}).
    %validate(#?MODULE{state = new}).

is_valid(_) -> false.
