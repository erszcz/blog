defmodule ContactInformation do
  defstruct [:address, :phone_number]

  @type t :: %__MODULE__{
    address: String.t(),
    phone_number: String.t() | nil
  }
end
