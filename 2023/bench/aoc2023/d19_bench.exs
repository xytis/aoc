defmodule AOC2023.D19Bench do
  use Benchfella

  @bench_data <<"""
  """>>

  bench "part1 on test data" do
    AOC2023.D19.part1(@bench_data)
  end

  bench "part2 on test data" do
    AOC2023.D19.part2(@bench_data)
  end
end
