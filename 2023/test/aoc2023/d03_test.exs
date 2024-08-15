defmodule AOC2023.D03Test do
  use ExUnit.Case
  doctest AOC2023.D03

  @test_p1 <<"""
             467..114..
             ...*......
             ..35..633.
             ......#...
             617*......
             .....+.58.
             ..592.....
             ......755.
             ...$.*....
             .664.598..
             """>>

  @test_p2 @test_p1

  test "part1 on test data" do
    assert AOC2023.D03.part1(@test_p1) == 4361
  end

  test "part2 on test data" do
    assert AOC2023.D03.part2(@test_p2) == 467_835
  end

  test "internals" do
    assert AOC2023.D03.scan("467..114..") == [{:number, 467, 0, 2}, {:number, 114, 5, 7}]
    assert AOC2023.D03.scan("617*......") == [{:number, 617, 0, 2}, {:symbol, "*", 3, 3}]

    assert AOC2023.D03.bounding_iterator(1..1, 1..1) == [
             {0, 0},
             {1, 0},
             {2, 0},
             {2, 1},
             {2, 2},
             {1, 2},
             {0, 2},
             {0, 1}
           ]
  end
end
