defmodule ElixirTypesTest do
  use ExUnit.Case
  doctest ElixirTypes

  test "greets the world" do
    assert ElixirTypes.hello() == :world
  end
end
