defmodule AOC2023.D20Bench do
  use Benchfella

  @bench_data <<"""
  """>>

  bench "part1 on test data" do
    AOC2023.D20.part1(@bench_data)
  end

  bench "part2 on test data" do
    AOC2023.D20.part2(@bench_data)
  end
end
