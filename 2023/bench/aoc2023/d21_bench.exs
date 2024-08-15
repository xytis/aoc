defmodule AOC2023.D21Bench do
  use Benchfella

  @bench_data <<"""
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
  bench "part1 on test data" do
    AOC2023.D21.part1(@bench_data, 64)
  end

  bench "part2 on test data" do
    AOC2023.D21.part2(@bench_data, 64)
  end
end
