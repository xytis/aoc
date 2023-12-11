defmodule AOC2023.D10Test do
  use ExUnit.Case
  doctest AOC2023.D10

  @test_simple <<"""
                 .....
                 .S-7.
                 .|.|.
                 .L-J.
                 .....
                 """>>

  @test_p1_1 <<"""
               -L|F7
               7S-7|
               L|7||
               -L-J|
               L|-JF
               """>>

  @test_p1_2 <<"""
               7-F7-
               .FJ|7
               SJLL7
               |F--J
               LJ.LJ
               """>>

  @test_p2_1 <<"""
               ...........
               .S-------7.
               .|F-----7|.
               .||.....||.
               .||.....||.
               .|L-7.F-J|.
               .|..|.|..|.
               .L--J.L--J.
               ...........
               """>>

  @test_p2_2 <<"""
               FF7FSF7F7F7F7F7F---7
               L|LJ||||||||||||F--J
               FL-7LJLJ||||||LJL-77
               F--JF--7||LJLJ7F7FJ-
               L---JF-JLJ.||-FJLJJ7
               |F|F-JF---7F7-L7L|7|
               |FFJF7L7F-JF7|JL---7
               7-L-JL7||F7|L7F-7F7|
               L.L7LFJ|||||FJL7||LJ
               L7JLJL-JLJLJL--JLJ.L
               """>>

  test "part1 on test data" do
    assert AOC2023.D10.part1(@test_simple) == 4
    assert AOC2023.D10.part1(@test_p1_1) == 4
    assert AOC2023.D10.part1(@test_p1_2) == 8
  end

  @tag timeout: :infinity
  test "part2 on test data" do
    assert AOC2023.D10.part2(@test_simple) == 1
    assert AOC2023.D10.part2(@test_p2_1) == 4
    assert AOC2023.D10.part2(@test_p2_2) == 10
  end

  test "internals" do
    map = %{
      :start => {1, 1},
      {0, 0} => {:ground},
      {0, 1} => {:ground},
      {0, 2} => {:ground},
      {0, 3} => {:ground},
      {0, 4} => {:ground},
      {1, 0} => {:ground},
      {1, 2} => {"N", "S"},
      {1, 3} => {"N", "E"},
      {1, 4} => {:ground},
      {2, 0} => {:ground},
      {2, 1} => {"E", "W"},
      {2, 2} => {:ground},
      {2, 3} => {"E", "W"},
      {2, 4} => {:ground},
      {3, 0} => {:ground},
      {3, 1} => {"S", "W"},
      {3, 2} => {"N", "S"},
      {3, 3} => {"N", "W"},
      {3, 4} => {:ground},
      {4, 0} => {:ground},
      {4, 1} => {:ground},
      {4, 2} => {:ground},
      {4, 3} => {:ground},
      {4, 4} => {:ground}
    }

    assert AOC2023.D10.parse(@test_simple) == map

    {start, map} = AOC2023.D10.extract_start(map)

    [_ | south] = AOC2023.D10.walk(map, [{{1, 1}, "S", []}])
    [_ | east] = AOC2023.D10.walk(map, [{{1, 1}, "E", []}])
    south = Enum.map(south, fn {coords, _movement} -> coords end)
    east = Enum.map(east, fn {coords, _movement} -> coords end)
    assert south == Enum.reverse(east)
  end

  test "fill generation" do
    assert AOC2023.D10.generate_exits("|", {"N", {"I", "O"}}) == [
             {"E", {"O", "O"}},
             {"S", {"I", "O"}},
             {"W", {"I", "I"}}
           ]

    assert AOC2023.D10.generate_exits("|", {"E", {"I", "I"}}) == [
             {"S", {"O", "I"}},
             {"W", {"O", "O"}},
             {"N", {"O", "I"}}
           ]

    assert AOC2023.D10.generate_exits("|", {"S", {"I", "O"}}) == [
             {"W", {"I", "I"}},
             {"N", {"I", "O"}},
             {"E", {"O", "O"}}
           ]

    assert AOC2023.D10.generate_exits("|", {"W", {"I", "I"}}) == [
             {"N", {"I", "O"}},
             {"E", {"O", "O"}},
             {"S", {"I", "O"}}
           ]
  end

  test "map clean" do
    map = %{
      {0, 0} => {"N", "S"},
      {0, 1} => {"E", "W"}
    }

    path = [{0, 0}]

    assert AOC2023.D10.clean_map({map, path}) == %{
             {0, 0} => {"N", "S"},
             {0, 1} => {:ground}
           }
  end
end
