defmodule BuildPipeline.Run.AnalyseSelfWorth.BuildPipelineRun do
  alias BuildPipeline.Run.AnalyseSelfWorth.RunSecondBuildPipelineInstance

  def run(command_line_args) do
    start_time = DateTime.utc_now()

    {output, exit_code} = RunSecondBuildPipelineInstance.run(["run" | command_line_args])

    end_time = DateTime.utc_now()

    if exit_code == 0 do
      {:ok, DateTime.diff(end_time, start_time, :microsecond)}
    else
      {:error, output}
    end
  end
end
