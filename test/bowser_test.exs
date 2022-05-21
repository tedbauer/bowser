defmodule BowserTest do
  use ExUnit.Case
  doctest Bowser

  test "greets the world" do
    assert Bowser.hello() == :world
  end
end
