defmodule AOC2023.D17Bench do
  use Benchfella

  @bench_data <<"""
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

  bench "part1 on test data" do
    AOC2023.D17.part1(@bench_data)
  end

  bench "part2 on test data" do
    AOC2023.D17.part2(@bench_data)
  end
end
