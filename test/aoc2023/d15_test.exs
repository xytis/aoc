defmodule AOC2023.D15Test do
  use ExUnit.Case
  doctest AOC2023.D15

  @test_p1 <<"""
             rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
             """>>

  test "part1 on test data" do
    assert AOC2023.D15.part1(@test_p1) == 1320
  end

  test "part2 on test data" do
    assert AOC2023.D15.part2(@test_p1) == 145
  end

  test "internals" do
    assert AOC2023.D15.parse("rn=1,cm-") == ["rn=1", "cm-"]
    assert AOC2023.D15.hash("HASH") == 52
    assert AOC2023.D15.hash("rn=1") == 30
    assert AOC2023.D15.hash("cm-") == 253
    assert AOC2023.D15.hash("qp=3") == 97
    assert AOC2023.D15.hash("cm=2") == 47
    assert AOC2023.D15.hash("qp-") == 14
    assert AOC2023.D15.hash("pc=4") == 180
    assert AOC2023.D15.hash("ot=9") == 9
    assert AOC2023.D15.hash("ab=5") == 197
    assert AOC2023.D15.hash("pc-") == 48
    assert AOC2023.D15.hash("pc=6") == 214
    assert AOC2023.D15.hash("ot=7") == 231
    assert AOC2023.D15.hash("rn") == 0
    assert AOC2023.D15.hash("qp") == 1
  end

  test "tokenify" do
    assert AOC2023.D15.tokenify("pc=6") == {3, :assign, :pc, 6}
    assert AOC2023.D15.tokenify("rn-") == {0, :remove, :rn}
  end

  test "example" do
    boxes = %{}

    import AOC2023.D15

    boxes = perform(tokenify("rn=1"), boxes)
    assert boxes == %{0 => [rn: 1]}

    boxes = perform(tokenify("cm-"), boxes)
    assert boxes == %{0 => [rn: 1]}

    boxes = perform(tokenify("qp=3"), boxes)

    assert boxes == %{
             0 => [rn: 1],
             1 => [qp: 3]
           }

    boxes = perform(tokenify("cm=2"), boxes)

    assert boxes == %{
             0 => [rn: 1, cm: 2],
             1 => [qp: 3]
           }

    boxes = perform(tokenify("qp-"), boxes)

    assert boxes == %{
             0 => [rn: 1, cm: 2],
             1 => []
           }

    boxes = perform(tokenify("pc=4"), boxes)

    assert boxes == %{
             0 => [rn: 1, cm: 2],
             3 => [pc: 4],
             1 => []
           }

    boxes = perform(tokenify("ot=9"), boxes)

    assert boxes == %{
             0 => [rn: 1, cm: 2],
             3 => [pc: 4, ot: 9],
             1 => []
           }

    boxes = perform(tokenify("ab=5"), boxes)

    assert boxes == %{
             0 => [rn: 1, cm: 2],
             3 => [pc: 4, ot: 9, ab: 5],
             1 => []
           }

    boxes = perform(tokenify("pc-"), boxes)

    assert boxes == %{
             0 => [rn: 1, cm: 2],
             3 => [ot: 9, ab: 5],
             1 => []
           }

    boxes = perform(tokenify("pc=6"), boxes)

    assert boxes == %{
             0 => [rn: 1, cm: 2],
             3 => [ot: 9, ab: 5, pc: 6],
             1 => []
           }

    boxes = perform(tokenify("ot=7"), boxes)

    assert boxes == %{
             0 => [rn: 1, cm: 2],
             3 => [ot: 7, ab: 5, pc: 6],
             1 => []
           }
  end
end
