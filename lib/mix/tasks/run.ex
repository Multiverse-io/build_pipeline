defmodule Mix.Tasks.BuildPipeline.Run do
  @moduledoc """
  Runs the build_pipline as defined in the `config.json` file, which was generated but the mix task `Init`.
  See the readme for more information on how to run and get setup
  """
  @shortdoc "Runs the build_pipeline steps as defined in build_pipeline's config.json file"

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    BuildPipeline.main(args)
  end
end
