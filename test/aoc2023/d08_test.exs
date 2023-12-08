defmodule AOC2023.D08Test do
  use ExUnit.Case
  doctest AOC2023.D08

  @test_p1 <<"""
             LLR

             AAA = (BBB, BBB)
             BBB = (AAA, ZZZ)
             ZZZ = (ZZZ, ZZZ)
             """>>

  @test_p2 <<"""
             LR

             11A = (11B, XXX)
             11B = (XXX, 11Z)
             11Z = (11B, XXX)
             22A = (22B, XXX)
             22B = (22C, 22C)
             22C = (22Z, 22Z)
             22Z = (22B, 22B)
             XXX = (XXX, XXX)
             """>>

  test "part1 on test data" do
    assert AOC2023.D08.part1(@test_p1) == 6
  end

  test "part2 on test data" do
    assert AOC2023.D08.part2(@test_p2) == 6
  end

  test "internals" do
    assert AOC2023.D08.match("AAA = (BBB, BBB)") == {"AAA", {"BBB", "BBB"}}

    assert AOC2023.D08.parse(@test_p1) == {
             ["L", "L", "R"],
             %{
               "AAA" => {"BBB", "BBB"},
               "BBB" => {"AAA", "ZZZ"},
               "ZZZ" => {"ZZZ", "ZZZ"}
             }
           }
  end
end
