defmodule Mix.Tasks.Day do
  @moduledoc "The hello mix task: `mix help hello`"
  use Mix.Task

  @year "aoc2023"

  def last_day() do
    Path.wildcard("lib/#{@year}/d*")
    |> Enum.map(fn path -> Path.basename(path, ".ex") end)
    |> Enum.filter(fn name -> String.match?(name, ~r/d\d\d/) end)
    |> Enum.sort(:desc)
    |> List.first()
    |> case do
      nil -> 0
      name -> String.slice(name, 1..2) |> String.to_integer()
    end
  end

  @shortdoc "Creates a next day template"
  def run(_args) do
    last_day()
    |> then(fn day ->
      day = Integer.to_string(day + 1)
      module_name = "D#{String.pad_leading(day, 2, "0")}"
      file_name = "d#{String.pad_leading(day, 2, "0")}"

      Mix.Generator.copy_template(
        "lib/mix/tasks/source/day.ex.heex",
        "lib/#{@year}/#{file_name}.ex",
        name: module_name,
        day: day,
        test_input_name: "input/#{file_name}.txt"
      )

      Mix.Generator.copy_template(
        "lib/mix/tasks/source/day_test.exs.heex",
        "test/#{@year}/#{file_name}_test.exs",
        name: module_name
      )
    end)
  end
end
