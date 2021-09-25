-module(examples).
-gradualizer([infer]).

%% If Gradualizer warnings don't show up make sure you have Gradualizer ebin/ on your code path.
%% For example:
%%
%%   $ export ERL_LIBS=/Users/erszcz/work/erszcz
%%
%% Given /Users/erszcz/work/erszcz/gradualizer/ebin exists and is populated.

-export([bad_initialization/0]).

-spec bad_initialization() -> integer().
bad_initialization() ->
    %MyContactInformation = #{phone_number => 123},
    MyContactInformation = contact_information(123),
    phone_service:call(MyContactInformation).

-spec contact_information(binary()) -> contact_information:t().
contact_information(B) ->
    #{phone_number => B}.

-type contact_information() :: #{address := binary(),
                                 phone_number => binary() | undefined}.

-type typed_map() :: #{field := binary()}.

-spec f() -> typed_map().
f() ->
    M = #{field => 123},
    g(M).

-spec g(typed_map()) -> typed_map().
g(TM) -> TM.

-spec bad_initialization2() -> integer().
bad_initialization2() ->
    MyContactInformation = #{phone_number => 123},
    call(MyContactInformation).

-spec call(contact_information()) -> integer().
call(Contact) ->
    %% unpack the contact info
    URL = <<"http://example.com/call/", (maps:get(phone_number, Contact))/bytes>>,
    {ok, Status} = post(URL),
    Status.

-spec post(binary()) -> {ok, integer()}.
post(URL) when is_binary(URL) ->
    %% some external call happens here
    {ok, 200}.
