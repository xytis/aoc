defmodule AOC2023.D17Test do
  use ExUnit.Case
  doctest AOC2023.D17

  @test_p1 <<"""
             8413432311323
             3215453535623
             3255245654254
             3446585845452
             4546657867536
             1438598798454
             4457876987766
             3637877979653
             4654967986887
             4564679986453
             1224686865563
             2546548887735
             4322674655533
             """>>

  @test_p2 <<"""
             111111111111
             999999999991
             999999999991
             999999999991
             999999999991
             """>>

  test "part1 on test data" do
    assert AOC2023.D17.part1(@test_p1) == 102
  end

  test "part2 on test data" do
    assert AOC2023.D17.part2(@test_p2) == 71
  end

  test "internals" do
    import AOC2023.D17
    assert parse("12\n34") == %{{0, 0} => 1, {1, 0} => 2, {0, 1} => 3, {1, 1} => 4}
  end
end
