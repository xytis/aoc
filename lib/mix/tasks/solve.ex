defmodule Mix.Tasks.Solve do
  @moduledoc "The hello mix task: `mix help hello`"
  use Mix.Task

  def run(args) do
    days =
      case args do
        [] -> [Mix.Tasks.Day.last_day() |> Integer.to_string()]
        ["all"] -> 1..Mix.Tasks.Day.last_day() |> Enum.map(&Integer.to_string/1)
        [day] -> [day]
      end

    Enum.each(days, fn day ->
      module =
        [
          AOC2023,
          "D#{String.pad_leading(day, 2, "0")}" |> String.to_atom()
        ]
        |> Module.concat()

      module.solve()
    end)
  end
end
