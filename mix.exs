defmodule BuildPipeline.MixProject do
  use Mix.Project

  @app_name :build_pipeline

  def project do
    [
      app: :build_pipeline,
      version: "0.1.0",
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: [main_module: BuildPipeline, name: escript_name(Mix.env())]
    ]
  end

  defp elixirc_paths(:test) do
    ["lib"]
  end

  defp elixirc_paths(_) do
    ["lib"]
  end

  defp escript_name(:prod) do
    @app_name
  end

  defp escript_name(env) do
    :"#{@app_name}_#{to_string(env)}"
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [{:jason, "~> 1.2"}]
  end
end
