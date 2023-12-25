defmodule AOC2023.D24Test do
  alias Tensor.Vector
  use ExUnit.Case
  doctest AOC2023.D24

  @test_p1 <<"""
             19, 13, 30 @ -2,  1, -2
             18, 19, 22 @ -1, -1, -2
             20, 25, 34 @ -2, -2, -4
             12, 31, 28 @ -1, -2, -1
             20, 19, 15 @  1, -5, -3
             """>>

  @test_p2 <<"""
             """>>

  test "part1 on test data" do
    assert AOC2023.D24.part1(@test_p1, 7, 27) == 2
  end

  test "part2 on test data" do
    assert AOC2023.D24.part2(@test_p2) == 0
  end

  test "internals" do
    import AOC2023.D24
    import Tensor

    alias AOC2023.D24.Box
    alias AOC2023.D24.Line

    assert parse_vector("19, 13, 30") == Vector.new([19, 13, 30])

    assert parse_line("19, 13, 30 @ -2,  1, -2") == %AOC2023.D24.Line{
             p1: Vector.new([19, 13, 30]),
             p2: Vector.new([17, 14, 28])
           }

    box = Box.new(Vector.new([7, 7, 0]), Vector.new([27, 27, 0]))
    l1 = Line.new(Vector.new([19, 13, 0]), Vector.new([17, 14, 0]))
    l2 = Line.new(Vector.new([18, 19, 0]), Vector.new([17, 18, 0]))
    assert Line.intersect_within?(l1, l2, box) == true
  end
end
