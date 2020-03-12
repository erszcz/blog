-module(phone_service).

-export([call/1]).

-spec call(contact_information:t()) -> integer().
call(Contact) ->
    %% unpack the contact info
    URL = <<"http://example.com/call/", (maps:get(phone_number, Contact))/bytes>>,
    {ok, Status} = post(URL),
    Status.

-spec post(binary()) -> {ok, integer()}.
post(URL) when is_binary(URL) ->
    %% some external call happens here
    {ok, 200}.
