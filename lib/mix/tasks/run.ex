defmodule Mix.Tasks.BuildPipeline.Run do
  @moduledoc ""
  @shortdoc "this doesn't work?"

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    BuildPipeline.main(args)
  end
end
