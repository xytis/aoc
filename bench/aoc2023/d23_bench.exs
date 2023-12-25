defmodule AOC2023.D23Bench do
  use Benchfella

  @bench_data <<"""
  """>>

  bench "part1 on test data" do
    AOC2023.D23.part1(@bench_data)
  end

  bench "part2 on test data" do
    AOC2023.D23.part2(@bench_data)
  end
end
