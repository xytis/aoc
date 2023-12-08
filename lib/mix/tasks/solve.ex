defmodule Mix.Tasks.Solve do
  @moduledoc "The hello mix task: `mix help hello`"
  use Mix.Task

  def run(args) do
    [day] = args

    module =
      [
        AOC2023,
        "D#{String.pad_leading(day, 2, "0")}" |> String.to_atom()
      ]
      |> Module.concat()

    module.solve()
  end
end
