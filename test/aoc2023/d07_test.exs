defmodule AOC2023.D07Test do
  use ExUnit.Case
  doctest AOC2023.D07

  @test_p1 <<"""
             32T3K 765
             T55J5 684
             KK677 28
             KTJJT 220
             QQQJA 483
             """>>

  @test_p2 @test_p1

  test "part1 on test data" do
    assert AOC2023.D07.part1(@test_p1) == 6440
  end

  test "part2 on test data" do
    assert AOC2023.D07.part2(@test_p2) == 5905
  end

  test "internals" do
    assert AOC2023.D07.parse("32T3K 765") == {[:one_pair, :_3, :_2, :_T, :_3, :_K], 765}
    assert AOC2023.D07.parse("T55J5 684") == {[:three_of_a_kind, :_T, :_5, :_5, :_J, :_5], 684}
    assert AOC2023.D07.parse("KK677 28") == {[:two_pairs, :_K, :_K, :_6, :_7, :_7], 28}
    assert AOC2023.D07.parse("KTJJT 220") == {[:two_pairs, :_K, :_T, :_J, :_J, :_T], 220}
    assert AOC2023.D07.parse("QQQJA 483") == {[:three_of_a_kind, :_Q, :_Q, :_Q, :_J, :_A], 483}
  end
end
