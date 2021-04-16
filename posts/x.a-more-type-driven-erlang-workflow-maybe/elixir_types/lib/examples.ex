defmodule Examples do

  def bad_initialization() do
    my_contact_information = %ContactInformation{}
    PhoneService.call(my_contact_information)
  end

end
