-module(contact_information).

-export_type([t/0]).

-type t() :: #{address := binary(),
               phone_number => binary() | undefined}.
