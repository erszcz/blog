defmodule PhoneService do

  @spec call(ContactInformation.t()) :: integer
  def call(contact) do
    url = "http://example.com/call/" <> contact.phone_number
    {:ok, status} = post(url)
    status
  end

  defp post(url) when is_binary(url) do
    # some external call happens here
    {:ok, 200}
  end
end
