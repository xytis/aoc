defmodule AOC2023.D01Test do
  use ExUnit.Case
  doctest AOC2023.D01

  @test_p1 <<"""
  1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet
  """>>

  @test_p2 <<"""
  two1nine
  eightwothree
  abcone2threexyz
  xtwone3four
  4nineeightseven2
  zoneight234
  7pqrstsixteen
  """>>

  test "part1 on test data" do
    assert AOC2023.D01.part1(@test_p1) == Enum.sum([12, 38, 15, 77])
  end

  test "part2 on test data" do
    assert AOC2023.D01.part2(@test_p2) == Enum.sum([29, 83, 13, 24, 42, 14, 76])
  end

  test "internals" do
    assert AOC2023.D01.exclude_non_numeric("1abc2") == "12"
    assert AOC2023.D01.parse("1abc2") == [1, 2]
    assert AOC2023.D01.decode([1, 2]) == 12
    assert AOC2023.D01.parse("one") == [1]
    assert AOC2023.D01.parse("two") == [2]
    assert AOC2023.D01.parse("three1four") == [3, 1, 4]
    assert AOC2023.D01.parse("one2three") == [1, 2, 3]
    assert AOC2023.D01.parse("zoneight234") == [1, 8, 2, 3, 4]
    assert AOC2023.D01.parse("zoneight") == [1, 8]
    assert AOC2023.D01.parse("one2nope4five") == [1, 2, 4, 5]
  end
end
