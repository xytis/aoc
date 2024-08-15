defmodule AOC2023.D14Test do
  use ExUnit.Case
  doctest AOC2023.D14

  @test_p1 <<"""
             O....#....
             O.OO#....#
             .....##...
             OO.#O....O
             .O.....O#.
             O.#..O.#.#
             ..O..#O..O
             .......O..
             #....###..
             #OO..#....
             """>>

  @test_p2_cycle1 <<"""
                    .....#....
                    ....#...O#
                    ...OO##...
                    .OO#......
                    .....OOO#.
                    .O#...O#.#
                    ....O#....
                    ......OOOO
                    #...O###..
                    #..OO#....
                    """>>

  @test_p2_cycle2 <<"""
                    .....#....
                    ....#...O#
                    .....##...
                    ..O#......
                    .....OOO#.
                    .O#...O#.#
                    ....O#...O
                    .......OOO
                    #..OO###..
                    #.OOO#...O
                    """>>

  @test_p2_cycle3 <<"""
                    .....#....
                    ....#...O#
                    .....##...
                    ..O#......
                    .....OOO#.
                    .O#...O#.#
                    ....O#...O
                    .......OOO
                    #...O###.O
                    #.OOO#...O
                    """>>

  test "part1 on test data" do
    assert AOC2023.D14.part1(@test_p1) == 136
  end

  test "part2 on test data" do
    assert AOC2023.D14.part2(@test_p1) == 64
  end

  test "improved roll" do
    line = [".", "O", "#", "O"]
    assert AOC2023.D14.do_improved_roll({0, 0}, line) == ["O", ".", "#", "O"]
  end

  test "internals" do
    little =
      <<"""
        O..#
        ..O#
        #..O
        O.OO
        """>>

    parsed = AOC2023.D14.parse(little)

    assert parsed == [
             ["O", ".", ".", "#"],
             [".", ".", "O", "#"],
             ["#", ".", ".", "O"],
             ["O", ".", "O", "O"]
           ]

    transposed = AOC2023.D14.transpose(parsed)

    assert transposed == [
             ["O", ".", "#", "O"],
             [".", ".", ".", "."],
             [".", "O", ".", "O"],
             ["#", "#", "O", "O"]
           ]

    rotated = AOC2023.D14.rotate_anticlockwise(parsed)

    assert rotated == [
             ["#", "#", "O", "O"],
             [".", "O", ".", "O"],
             [".", ".", ".", "."],
             ["O", ".", "#", "O"]
           ]

    assert parsed ==
             parsed
             |> AOC2023.D14.rotate_anticlockwise()
             |> AOC2023.D14.rotate_anticlockwise()
             |> AOC2023.D14.rotate_anticlockwise()
             |> AOC2023.D14.rotate_anticlockwise()

    assert parsed ==
             parsed
             |> AOC2023.D14.rotate_clockwise()
             |> AOC2023.D14.rotate_clockwise()
             |> AOC2023.D14.rotate_clockwise()
             |> AOC2023.D14.rotate_clockwise()

    assert parsed ==
             parsed
             |> AOC2023.D14.rotate_clockwise()
             |> AOC2023.D14.rotate_anticlockwise()

    tilted = AOC2023.D14.tilt_left(transposed)

    assert tilted == [
             ["O", ".", "#", "O"],
             [".", ".", ".", "."],
             ["O", "O", ".", "."],
             ["#", "#", "O", "O"]
           ]
  end

  test "cycles" do
    parsed = AOC2023.D14.parse(@test_p1)
    cycle1 = AOC2023.D14.parse(@test_p2_cycle1)
    cycle2 = AOC2023.D14.parse(@test_p2_cycle2)
    cycle3 = AOC2023.D14.parse(@test_p2_cycle3)

    # Orient everything to north as left
    parsed = AOC2023.D14.rotate_anticlockwise(parsed)
    cycle1 = AOC2023.D14.rotate_anticlockwise(cycle1)
    cycle2 = AOC2023.D14.rotate_anticlockwise(cycle2)
    cycle3 = AOC2023.D14.rotate_anticlockwise(cycle3)

    assert cycle1 == AOC2023.D14.cycle(parsed)
    assert cycle2 == AOC2023.D14.cycle(cycle1)
    assert cycle3 == AOC2023.D14.cycle(cycle2)

    cycle4 = AOC2023.D14.cycle(cycle3)
    cycle5 = AOC2023.D14.cycle(cycle4)
    cycle6 = AOC2023.D14.cycle(cycle5)

    assert 64 == AOC2023.D14.count_total_load(cycle6)
  end
end
