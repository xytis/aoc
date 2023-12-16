defmodule AOC2023.D12Bench do
  use Benchfella

  @bench_data <<"""
  ???.### 1,1,3
  .??..??...?##. 1,1,3
  ?#?#?#?#?#?#?#? 1,3,1,6
  ????.#...#... 4,1,1
  ????.######..#####. 1,6,5
  ?###???????? 3,2,1
  """>>

  bench "part1 on test data" do
    AOC2023.D12.part1(@bench_data)
  end

  bench "part2 on test data" do
    AOC2023.D12.part2(@bench_data)
  end
end
