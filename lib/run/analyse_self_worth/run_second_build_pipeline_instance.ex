defmodule BuildPipeline.Run.AnalyseSelfWorth.RunSecondBuildPipelineInstance do
  def run(args) do
    System.cmd(bp_binary(), args)
  end

  #TODO use bp_dev in test & bp everywhere else?
  defp bp_binary do
    if Application.get_env(:build_pipeline, :env) == :test do
      "./bp"
    else
      "bp"
    end
  end
end
