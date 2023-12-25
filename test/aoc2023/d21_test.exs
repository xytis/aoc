defmodule AOC2023.D21Test do
  alias AOC2023.D21.Grid
  use ExUnit.Case
  doctest AOC2023.D21

  @test_data <<"""
               ...........
               .....###.#.
               .###.##..#.
               ..#.#...#..
               ....#.#....
               .##..S####.
               .##..#...#.
               .......##..
               .##.#.####.
               .##..##.##.
               ...........
               """>>
  @test_mini_data <<"""
                    ...........
                    ......##.#.
                    .###..#..#.
                    ..#.#...#..
                    ....#.#....
                    .....S.....
                    .##......#.
                    .......##..
                    .##.#.####.
                    .##...#.##.
                    ...........
                    """>>

  test "part1 on test data" do
    assert AOC2023.D21.part1(@test_data, 0) == 1
    assert AOC2023.D21.part1(@test_data, 1) == 2
    assert AOC2023.D21.part1(@test_data, 2) == 4
    assert AOC2023.D21.part1(@test_data, 3) == 6
    assert AOC2023.D21.part1(@test_data, 6) == 16
  end

  test "part1 on real data" do
    import AOC2023.D21
    input = read_input()
    assert part1(input, 64) == 3776
    assert part1(input, 132) == 7689
    assert part1(input, 133) == 7597
    assert part1(input, 1000) == 7689
    assert part1(input, 1001) == 7597
  end

  test "saturation on real data" do
    import AOC2023.D21
    input = read_input()

    {start, grid} =
      input
      |> Grid.parse()

    {even, odd, iteration} = saturate(grid, start)

    assert iteration == 131
    assert even == 7689
    assert odd == 7597

    {center_x, center_y} = start

    {^even, ^odd, tl_saturation} = saturate(grid, {grid.size - 1, grid.size - 1})
    assert tl_saturation == 131 + 131 - 1
    {^even, ^odd, tc_saturation} = saturate(grid, {center_x, grid.size - 1})
    assert tc_saturation == 131 + (131 - 1) / 2
    {^even, ^odd, tr_saturation} = saturate(grid, {0, grid.size - 1})
    assert tr_saturation == 131 + 131 - 1

    {^even, ^odd, cl_saturation} = saturate(grid, {grid.size - 1, center_y})
    assert cl_saturation == 131 + (131 - 1) / 2
    {^even, ^odd, cc_saturation} = saturate(grid, {center_x, center_y})
    assert cc_saturation == 131
    {^even, ^odd, cr_saturation} = saturate(grid, {0, center_y})
    assert cr_saturation == 131 + (131 - 1) / 2

    {^even, ^odd, bl_saturation} = saturate(grid, {grid.size - 1, 0})
    assert bl_saturation == 131 + 131 - 1
    {^even, ^odd, bc_saturation} = saturate(grid, {center_x, 0})
    assert bc_saturation == 131 + (131 - 1) / 2
    {^even, ^odd, br_saturation} = saturate(grid, {0, 0})
    assert br_saturation == 131 + 131 - 1
  end

  test "symetry on real data" do
    import AOC2023.D21
    input = read_input()

    {start, grid} =
      input
      |> Grid.parse()

    {center_x, center_y} = start

    assert calculate(grid, {center_x, grid.size - 1}, 131) != calculate(grid, {center_x, 0}, 131)
  end

  test "part2 on test data" do
    assert AOC2023.D21.part2_brute(@test_data, 6) == 16
    assert AOC2023.D21.part2_brute(@test_data, 10) == 50
    assert AOC2023.D21.part2_brute(@test_data, 16) == 50
    assert AOC2023.D21.part2(@test_data, 50) == 1594
    assert AOC2023.D21.part2(@test_data, 100) == 6536
    assert AOC2023.D21.part2(@test_data, 500) == 167_004
    assert AOC2023.D21.part2(@test_data, 1000) == 668_697
    assert AOC2023.D21.part2(@test_data, 5000) == 16_733_044
  end

  @tag timeout: :infinity
  test "part2 on better test data" do
    assert AOC2023.D21.part2(@test_mini_data, 16) == 216
    assert AOC2023.D21.part2(@test_mini_data, 50) == 1940
    assert AOC2023.D21.part2(@test_mini_data, 100) == 7645
    assert AOC2023.D21.part2(@test_mini_data, 500) == 188_756
    assert AOC2023.D21.part2(@test_mini_data, 1000) == 753_480
    assert AOC2023.D21.part2(@test_mini_data, 5000) == 18_807_440
  end

  test "part2 brute on real data" do
    import AOC2023.D21
    input = read_input()
    assert part2_brute(input, 65) == 3884
    assert part2_brute(input, 196) == 34564
    assert part2_brute(input, 327) == 95816
    # assert part2_brute(input, 458) == 187_640
    # assert part2_brute(input, 1375) == 1_686_424
    # assert part2_brute(input, 1637) == 2_389_796
    # assert part2_brute(input, 1768) == 2_787_340

    # 628545641525613 -> too high
    # 625584859509375 -> too low
    # assert part2(input, 26_501_365)
  end

  test "part2 good on real data" do
    import AOC2023.D21
    input = read_input()
    assert part2(input, 458) == 187_640
    assert part2(input, 1375) == 1_686_424
    assert part2(input, 1637) == 2_389_796
    assert part2(input, 1768) == 2_787_340
  end

  test "internals" do
    import AOC2023.D21

    {start, grid} = Grid.parse(".#\nS.")

    assert start == {0, 1}

    assert grid ==
             %Grid{
               grid: %{{0, 0} => :garden, {1, 0} => :rock, {0, 1} => :garden, {1, 1} => :garden},
               size: 2,
               infinite: false
             }

    assert Grid.fetch(grid, {-4, 0}) == :error
    assert Grid.fetch(grid, {-3, 0}) == :error
    assert Grid.fetch(grid, {-2, 0}) == :error
    assert Grid.fetch(grid, {-1, 0}) == :error
    assert Grid.fetch(grid, {0, 0}) == {:ok, :garden}
    assert Grid.fetch(grid, {1, 0}) == {:ok, :rock}
    assert Grid.fetch(grid, {2, 0}) == :error
    assert Grid.fetch(grid, {3, 0}) == :error
    assert Grid.fetch(grid, {4, 0}) == :error

    grid = Grid.make_infinite(grid)

    assert Grid.fetch(grid, {-4, 0}) == {:ok, :garden}
    assert Grid.fetch(grid, {-3, 0}) == {:ok, :rock}
    assert Grid.fetch(grid, {-2, 0}) == {:ok, :garden}
    assert Grid.fetch(grid, {-1, 0}) == {:ok, :rock}
    assert Grid.fetch(grid, {0, 0}) == {:ok, :garden}
    assert Grid.fetch(grid, {1, 0}) == {:ok, :rock}
    assert Grid.fetch(grid, {2, 0}) == {:ok, :garden}
    assert Grid.fetch(grid, {3, 0}) == {:ok, :rock}
    assert Grid.fetch(grid, {4, 0}) == {:ok, :garden}
  end
end
