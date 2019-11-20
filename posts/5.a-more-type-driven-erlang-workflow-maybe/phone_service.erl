-module(phone_service).

-export([call/1]).

-spec call(contact_information:t()) -> integer().
call(Contact) ->
    %% unpack the contact info
    #{phone_number := PN} = Contact,
    URL = <<"http://example.com/call/", PN/bytes>>,
    {ok, Status} = post(URL),
    Status.

-spec post(binary()) -> {ok, integer()}.
post(URL) when is_binary(URL) ->
    %% some external call happens here
    {ok, 200}.
