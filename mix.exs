defmodule BuildPipeline.MixProject do
  use Mix.Project

  @app_name :build_pipeline
  @escript_name :"run_#{@app_name}"

  def project do
    [
      app: :build_pipeline,
      description: description(),
      package: package(),
      version: "0.1.0",
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: [main_module: BuildPipeline, name: escript_name(Mix.env())]
    ]
  end

  defp package do
    [
      name: "build_pipeline",
      licenses: ["WTFPL"],
      links: %{"GitHub" => "https://github.com/mbernerslee/build_pipeline"},
      source_url: "https://github.com/mbernerslee/build_pipeline"
    ]
  end

  defp description do
    "A development tool for running commands with maximum possible concurrency, designed to speed up CI / CD build piplines by running mulitple independent build steps at once."
  end

  defp elixirc_paths(:test) do
    ["lib", "test/builders"]
  end

  defp elixirc_paths(_) do
    ["lib"]
  end

  defp escript_name(:prod) do
    @escript_name
  end

  defp escript_name(env) do
    :"#{@escript_name}_#{to_string(env)}"
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.27", only: :dev, runtime: false},
      {:jason, "~> 1.2"}
    ]
  end
end
