defmodule AOC2023.D16Test do
  use ExUnit.Case
  doctest AOC2023.D16

  @test_p1 ~S"""
  .|...\....
  |.-.\.....
  .....|-...
  ........|.
  ..........
  .........\
  ..../.\\..
  .-.-/..|..
  .|....-|.\
  ..//.|....
  """

  test "part1 on test data" do
    assert AOC2023.D16.part1(@test_p1) == 46
  end

  test "part2 on test data" do
    assert AOC2023.D16.part2(@test_p1) == 51
  end

  test "internals" do
    import AOC2023.D16
    assert map_size(parse(@test_p1)) == 100

    grid =
      parse(~S"""
      .-\
      .-.
      .|/
      """)

    grid = propagate(grid, {0, 0}, ">")

    assert count_energised(grid) == 8
  end
end
