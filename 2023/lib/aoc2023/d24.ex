defmodule AOC2023.D24 do
  @moduledoc """
  Documentation for `AOC2023.D24`.
  """

  defmodule Box do
    defstruct [:p1, :p2]

    def new(p1, p2) do
      %__MODULE__{
        p1: p1,
        p2: p2
      }
    end

    def inside?(%__MODULE__{} = box, %Tensor.Tensor{} = p) do
      px = p[0]
      py = p[1]
      pz = p[2]
      b1x = box.p1[0]
      b1y = box.p1[1]
      b1z = box.p1[2]
      b2x = box.p2[0]
      b2y = box.p2[1]
      b2z = box.p2[2]

      b1x <= px && px <= b2x && b1y <= py && py <= b2y && b1z <= pz && pz <= b2z
    end
  end

  defmodule Line do
    defstruct [:p1, :p2]

    @epsilon 0.00001

    use Memoize

    def new(p1, p2) do
      %__MODULE__{
        p1: p1,
        p2: p2
      }
    end

    @spec from_position_velocity(Tensor.Tensor.tensor(), any()) :: %AOC2023.D24.Line{
            p1: Tensor.Tensor.tensor(),
            p2: Tensor.Tensor.tensor()
          }
    def from_position_velocity(p, v) do
      %__MODULE__{
        p1: p,
        p2: Tensor.Vector.add(p, v)
      }
    end

    def project_xy(%__MODULE__{} = line) do
      use Tensor

      %__MODULE__{
        p1: Vector.new([line.p1[0], line.p1[1], 0]),
        p2: Vector.new([line.p2[0], line.p2[1], 0])
      }
    end

    # dmnop = (xm - xn)(xo - xp) + (ym - yn)(yo - yp) + (zm - zn)(zo - zp)
    def d(m, n, o, p) do
      res =
        (m[0] - n[0]) * (o[0] - p[0]) +
          (m[1] - n[1]) * (o[1] - p[1]) +
          (m[2] - n[2]) * (o[2] - p[2])

      res
    end

    # mua = ( d1343 d4321 - d1321 d4343 ) / ( d2121 d4343 - d4321 d4321 )
    def mua(p1, p2, p3, p4) do
      number =
        d(p1, p3, p4, p3) * d(p4, p3, p2, p1) - d(p1, p3, p2, p1) * d(p4, p3, p4, p3)

      denominator =
        d(p2, p1, p2, p1) * d(p4, p3, p4, p3) - d(p4, p3, p2, p1) * d(p4, p3, p2, p1)

      if(denominator < @epsilon) do
        :error
      else
        number / denominator
      end
    end

    # mub = ( d1343 + mua d4321 ) / d4343
    def mub(p1, p2, p3, p4, mua) do
      (d(p1, p3, p4, p3) + mua * d(p4, p3, p2, p1)) / d(p4, p3, p4, p3)
    end

    def closest_points(%__MODULE__{} = l1, %__MODULE__{} = l2) do
      p1 = l1.p1
      p2 = l1.p2
      p3 = l2.p1
      p4 = l2.p2

      alias Tensor.Vector

      ma = mua(p1, p2, p3, p4)

      cond do
        ma == :error ->
          :error

        ma > 0 ->
          # Pa = P1 + mua (P2 - P1)

          mb = mub(p1, p2, p3, p4, ma)

          if mb < 0 do
            :error
          else
            pa =
              Vector.add(
                p1,
                Vector.mult(
                  Vector.sub(p2, p1),
                  ma
                )
              )

            # Pb = P3 + mub (P4 - P3)
            pb =
              Vector.add(
                p3,
                Vector.mult(
                  Vector.sub(p4, p3),
                  mb
                )
              )

            {pa, pb}
          end

        true ->
          :error
      end
    end

    def intersect_within?(%__MODULE__{} = l1, %__MODULE__{} = l2, %Box{} = box) do
      case closest_points(l1, l2) do
        :error ->
          false

        {pa, pb} ->
          case vector_length(Vector.sub(pa, pb)) do
            x when is_number(x) and x < @epsilon -> Box.inside?(box, pa)
            _ -> false
          end
      end
    end

    defp vector_length(vec) do
      vec
      |> Vector.map(fn x -> x * x end)
      |> Enum.sum()
      |> :math.sqrt()
    end
  end

  defmodule VectorLine do
    alias Tensor.Vector
    defstruct [:p, :v]

    def new(p, v) do
      %__MODULE__{
        p: p,
        v: v
      }
    end

    def from_position_velocity(p, v) do
      new(p, v)
    end

    def project_xy(%__MODULE__{} = line) do
      %__MODULE__{
        p: Vector.new([line.p[0], line.p[1], 0]),
        v: Vector.new([line.v[0], line.v[1], 0])
      }
    end

    def intersect_within?(%__MODULE__{} = l1, %__MODULE__{} = l2, %Box{} = box) do
      alias Tensor.Vector

      mu = [
        [l1.v[0], -l2.v[0]],
        [l1.v[1], -l2.v[1]]
      ]

      mx = [
        [l2.p[0] - l1.p[0]],
        [l2.p[1] - l1.p[1]]
      ]

      a = MatrixOperation.cramer(mu, mx, 1)
      b = MatrixOperation.cramer(mu, mx, 2)

      case {a, b} do
        {nil, _} ->
          false

        {_, nil} ->
          false

        {a, b} when a > 0 and b > 0 ->
          p = Vector.add(l1.p, Vector.mult(l1.v, a))
          {a, b, Box.inside?(box, p)}

        _ ->
          false
      end
    end
  end

  def solve() do
    IO.puts("Day 24")
    input = read_input()
    IO.puts("Part 1: #{part1(input, 200_000_000_000_000, 400_000_000_000_000)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d24.txt")
  end

  @spec part1(any(), integer(), integer()) :: none()
  def part1(input, lim1, lim2) do
    lines =
      parse(input)
      |> Enum.map(fn line -> VectorLine.project_xy(line) end)
      |> IO.inspect()

    alias Tensor.Vector

    bounds = Box.new(Vector.new([lim1, lim1, 0]), Vector.new([lim2, lim2, 0]))

    for l1 <- lines, l2 <- lines do
      {l1, l2}
    end
    |> Enum.filter(fn {l1, l2} -> l1 != l2 end)
    |> Enum.filter(fn {l1, l2} ->
      case VectorLine.intersect_within?(l1, l2, bounds) do
        false ->
          false

        {_, _, inside} ->
          inside
      end
    end)
    |> Enum.count()
    |> div(2)
  end

  def part2(input) do
    2
  end

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end

  def parse_line(line) do
    line
    |> String.split("@", trim: true, parts: 2)
    |> then(fn [position_string, velocity_string] ->
      VectorLine.from_position_velocity(
        parse_vector(position_string),
        parse_vector(velocity_string)
      )
    end)
  end

  def parse_vector(vector_string) do
    alias Tensor.Vector

    vector_string
    |> String.split(",", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.to_integer/1)
    |> Vector.new()
  end
end
