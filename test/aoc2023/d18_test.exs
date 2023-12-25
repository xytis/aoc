defmodule AOC2023.D18Test do
  use ExUnit.Case
  doctest AOC2023.D18

  @test_p1 <<"""
             R 6 (#70c710)
             D 5 (#0dc571)
             L 2 (#5713f0)
             D 2 (#d2c081)
             R 2 (#59c680)
             D 2 (#411b91)
             L 5 (#8ceee2)
             U 2 (#caa173)
             L 1 (#1b58a2)
             U 2 (#caa171)
             R 2 (#7807d2)
             U 3 (#a77fa3)
             L 2 (#015232)
             U 2 (#7a21e3)
             """>>

  @test_little <<"""
                 R 9 (#70c710)
                 D 3 (#0dc571)
                 L 9 (#5713f0)
                 U 3 (#d2c081)
                 """>>

  @test_touching <<"""
                   R 4 (#70c710)
                   U 2 (#0dc571)
                   L 2 (#5713f0)
                   D 1 (#d2c081)
                   L 2 (#59c680)
                   U 2 (#411b91)
                   R 5 (#8ceee2)
                   D 5 (#caa173)
                   L 5 (#1b58a2)
                   U 2 (#caa171)
                   """>>

  @test_thin <<"""
               R 2 (#70c710)
               D 2 (#0dc571)
               R 1 (#5713f0)
               U 2 (#d2c081)
               R 2 (#59c680)
               D 4 (#411b91)
               L 5 (#8ceee2)
               U 4 (#caa173)
               """>>

  @test_inverted <<"""
                   R 4 (#70c710)
                   U 2 (#0dc571)
                   L 2 (#5713f0)
                   D 1 (#d2c081)
                   L 2 (#59c680)
                   U 2 (#411b91)
                   R 5 (#8ceee2)
                   D 5 (#caa173)
                   L 5 (#1b58a2)
                   U 2 (#caa171)
                   """>>
                 |> String.split("\n", trim: true)
                 |> Enum.reverse()
                 |> Enum.join("\n")
                 |> IO.inspect()

  # ##########
  # #~~~~~~~~#
  # #~~~~~~~~#
  # ##########
  test "part1 on test data" do
    assert AOC2023.D18.part1(@test_p1) == 62
  end

  test "part2 on test data" do
    assert AOC2023.D18.part2(@test_p1) == 952_408_144_115
  end

  test "part1 on little data" do
    assert AOC2023.D18.part1(@test_little) == 40
  end

  test "part1 on touching data" do
    assert AOC2023.D18.part1(@test_touching) == 35
  end

  test "part1 on thin data" do
    assert AOC2023.D18.part1(@test_thin) == 30
  end

  test "part1 on inverted data" do
    assert AOC2023.D18.part1(@test_inverted) == 30
  end

  test "internals" do
    import AOC2023.D18
  end
end
