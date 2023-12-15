defmodule AOC2023.D12Test do
  use ExUnit.Case
  doctest AOC2023.D12

  @test_p1 <<"""
             ???.### 1,1,3
             .??..??...?##. 1,1,3
             ?#?#?#?#?#?#?#? 1,3,1,6
             ????.#...#... 4,1,1
             ????.######..#####. 1,6,5
             ?###???????? 3,2,1
             """>>

  test "part1 on test data" do
    assert AOC2023.D12.part1(@test_p1) == 21
  end

  test "part2 on test data" do
    assert AOC2023.D12.part2(@test_p1) == 525_152
  end

  test "internals" do
    assert AOC2023.D12.parse("???.### 1,1,3") == [
             {
               ["?", "?", "?", ".", "#", "#", "#"],
               [1, 1, 3]
             }
           ]

    assert AOC2023.D12.is_valid?(["#", ".", "#", ".", "#", "#", "#"], [1, 1, 3]) == true
    assert AOC2023.D12.is_valid?(["#", "#", "#", ".", "#", "#", "#"], [1, 1, 3]) == false
    assert AOC2023.D12.is_valid?(["#", "#", ".", ".", "#", "#", "#"], [1, 1, 3]) == false
    assert AOC2023.D12.is_valid?([".", ".", ".", ".", "#", "#", "#"], [1, 1, 3]) == false
    assert AOC2023.D12.is_valid?([".", ".", ".", ".", "#", "#", "#"], [3, 1, 1]) == false
    assert AOC2023.D12.is_valid?(["#", ".", "#", ".", "#", ".", "."], [3]) == false

    assert AOC2023.D12.count_valid(["#", ".", "#", ".", "#", "#", "#"], [1, 1, 3]) == 1
    assert AOC2023.D12.count_valid(["#", "#", "#", ".", "#", "#", "#"], [1, 1, 3]) == 0
    assert AOC2023.D12.count_valid(["#", "#", ".", ".", "#", "#", "#"], [1, 1, 3]) == 0
    assert AOC2023.D12.count_valid([".", ".", ".", ".", "#", "#", "#"], [1, 1, 3]) == 0
    assert AOC2023.D12.count_valid([".", ".", ".", ".", "#", "#", "#"], [3, 1, 1]) == 0
    assert AOC2023.D12.count_valid(["#", ".", "#", ".", "#", ".", "."], [3]) == 0
    assert AOC2023.D12.count_valid(["?", "?", "?", ".", "#", "#", "#"], [1, 1, 3]) == 1

    assert AOC2023.D12.count_valid(["?", "?", ".", "?", "?", ".", "#"], [1, 1, 1]) == 4
  end

  test "long" do
    assert AOC2023.D12.parse("?###???????? 3,2,1", 5)
           |> Enum.map(fn {springs, pools} ->
             {_, count} = AOC2023.D12.count_valid(%{}, springs, pools)
             count
           end) == [506_250]

    assert AOC2023.D12.parse("????? 1,2", 40)
           |> Enum.map(fn {springs, pools} ->
             {_, count} = AOC2023.D12.count_valid(%{}, springs, pools)
             count
           end) == [114_556_848_244_965_165_743_109_806_892_471]
  end
end
