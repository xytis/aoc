defmodule AOC2023.MixProject do
  use Mix.Project

  def project do
    [
      app: :AOC2023,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      # Force mix bench to work without MIX_ENV=bench
      preferred_cli_env: [
        bench: :bench,
        "bench.cmp": :bench,
        "bench.graph": :bench
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchfella, "~> 0.3.0", only: :bench},
      {:memoize, "~> 1.4"},
      {:tensor, "~> 2.0"},
      {:matrix_operation, "~> 0.5"}
    ]
  end
end
