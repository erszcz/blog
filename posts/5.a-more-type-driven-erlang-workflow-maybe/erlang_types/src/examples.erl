-module(examples).

-export([bad_initialization/0]).

-spec bad_initialization() -> integer().
bad_initialization() ->
    MyContactInformation = #{phone_number => 123},
    phone_service:call(MyContactInformation).
