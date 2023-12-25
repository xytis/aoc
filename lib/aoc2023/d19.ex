defmodule AOC2023.D19 do
  @moduledoc """
  Documentation for `AOC2023.D19`.
  """

  defmodule Part do
    defstruct [:x, :m, :a, :s]

    def from_line(line) do
      line = String.trim_leading(line, "{")
      line = String.trim_trailing(line, "}")

      [x, m, a, s] =
        String.split(line, ",", trim: true, parts: 4)
        |> Enum.map(&(String.split(&1, "=", trim: true, parts: 2) |> List.last()))

      %__MODULE__{
        x: String.to_integer(x),
        m: String.to_integer(m),
        a: String.to_integer(a),
        s: String.to_integer(s)
      }
    end
  end

  defmodule Workflow do
    defprotocol Step do
      def apply(step, part)
    end

    defmodule SureStep do
      defstruct [:next]
    end

    defmodule ConditionStep do
      defstruct [:key, :condition, :expectation, :next]

      def new(key, condition, expectation, next) do
        %__MODULE__{key: key, condition: condition, expectation: expectation, next: next}
      end

      def from_string(str, next) do
        <<key::binary-size(1), condition::binary-size(1), expectation::binary>> = str
        new(key, condition, String.to_integer(expectation), next)
      end
    end

    def step_from_string(str) do
      case String.split(str, ":", trim: true) do
        [condition, next] -> ConditionStep.from_string(condition, next)
        [next] -> %SureStep{next: next}
      end
    end

    defimpl Step, for: SureStep do
      def apply(%SureStep{} = step, %Part{} = part) do
        step.next
      end
    end

    defimpl Step, for: ConditionStep do
      def apply(%ConditionStep{} = step, %Part{} = part) do
        value = Map.fetch!(part, step.key)

        case {step.condition, step.expectation} do
          {">", expectation} when value > expectation -> step.next
          {"<", expectation} when value < expectation -> step.next
          _ -> nil
        end
      end
    end
  end

  def solve() do
    IO.puts("Day 19")
    input = read_input()
    IO.puts("Part 1: #{part1(input)}")
    IO.puts("Part 2: #{part2(input)}")
  end

  def read_input() do
    File.read!("input/d19.txt")
  end

  def part1(input) do
    input
    |> parse()
  end

  def part2(input) do
    2
  end

  def parse(input) do
    [workflows, parts] =
      input
      |> String.split("\n\n", trim: true, parts: 2)

    {parse_workflows(workflows), parse_parts(parts)}
  end

  def parse_parts(parts) do
    parts
    |> String.split("\n", trim: true)
    |> Enum.map(&Part.from_line/1)
  end

  def parse_workflows(workflows) do
    workflows =
      workflows
      |> String.split("\n", trim: true)
      |> Enum.map(&match_workflow/1)
      |> Map.new()

    workflows = build_workflows(workflows, "in")
  end

  def match_workflow(string) do
    [name, definition] =
      string
      |> String.split("{", trim: true, parts: 2)

    definition = String.trim(definition, "}")
    {name, definition}
  end

  def build_workflows(workflows, name) do
    case Map.fetch!(workflows, name) do
      definition ->
        definition
        |> String.split("\n", trim: true)
        |> Enum.map(&Workflow.step_from_string/1)
        |> Enum.with_index()
        |> Enum.reduce(%{}, fn {step, index}, acc ->
          Map.put(acc, index, step)
        end)
        |> Map.put(:name, name)
        |> Map.put(:steps, Enum.count(definition))
        |> Map.put(:index, 0)
        |> Map.put(:part, %{})
        |> Map.put(:next, nil)
        |> Map.put(:previous, nil)
        |> Map.put(:workflows, workflows)
        |> build_workflows(name)
    end
  end
end
