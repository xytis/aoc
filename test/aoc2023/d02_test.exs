defmodule AOC2023.D02Test do
  use ExUnit.Case
  doctest AOC2023.D02

  @test_p1 <<"""
  Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
  Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
  Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
  """>>

  @test_p2 @test_p1

  test "part1 on test data" do
    assert AOC2023.D02.part1(@test_p1) == 8
  end

  test "part2 on test data" do
    assert AOC2023.D02.part2(@test_p2) == 2286
  end

  test "internals" do
    assert AOC2023.D02.parse("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") == {1, [[blue: 3, red: 4], [red: 1, green: 2, blue: 6], [green: 2]]}
    assert AOC2023.D02.parse("Game 405: 1 blue") == {405, [[blue: 1]]}
  end
end
