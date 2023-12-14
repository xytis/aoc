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

  @test_p2 <<"""
             """>>

  test "part1 on test data" do
    assert AOC2023.D12.part1(@test_p1) == 0
  end

  test "part2 on test data" do
    assert AOC2023.D12.part2(@test_p2) == 0
  end

  test "internals" do
    assert AOC2023.D12.parse("???.### 1,1,3") == [
             {
               [:unknown, :unknown, :unknown, :operational, :damaged, :damaged, :damaged],
               [1, 1, 3]
             }
           ]

    assert AOC2023.D12.is_valid_permutation?(
             [:operational, :damaged, :operational, :operational, :damaged, :damaged, :damaged],
             [:unknown, :unknown, :unknown, :operational, :damaged, :damaged, :damaged]
           )

    assert !AOC2023.D12.is_valid_permutation?(
             [:operational, :damaged, :operational, :damaged, :damaged, :damaged, :damaged],
             [:unknown, :unknown, :unknown, :operational, :damaged, :damaged, :damaged]
           )
  end
end
