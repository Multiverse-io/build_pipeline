defmodule BuildPipeline.Run.AnalyseSelfWorth.RunSecondBuildPipelineInstance do
  def run(args) do
    System.cmd(bp_binary(), args)
  end

  defp bp_binary do
    Application.get_env(:build_pipeline, :bp_binary)
  end
end
