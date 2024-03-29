defmodule BuildPipeline.MixProject do
  use Mix.Project

  @escript_name :bp

  def project do
    [
      app: :build_pipeline,
      description: description(),
      package: package(),
      version: "0.0.14",
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: [main_module: BuildPipeline, name: escript_name(Mix.env()), app: nil]
    ]
  end

  defp package do
    [
      name: "build_pipeline",
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/Multiverse-io/build_pipeline"},
      source_url: "https://github.com/Multiverse-io/build_pipeline"
    ]
  end

  defp description do
    "A development tool for running commands with maximum possible concurrency, designed to speed up CI / CD build piplines by running mulitple independent build steps at once."
  end

  defp elixirc_paths(:test) do
    ["lib", "test/run/builders", "test/run/mocks", "test/run/support"]
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
      {:jason, "~> 1.2"},
      {:mimic, "~> 1.7.4", only: :test},
      {:faker, "~> 0.17", only: :test}
    ]
  end
end
