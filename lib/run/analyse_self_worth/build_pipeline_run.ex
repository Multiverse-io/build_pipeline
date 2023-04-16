defmodule BuildPipeline.Run.AnalyseSelfWorth.BuildPipelineRun do
  def run(command_line_args) do
    start_time = DateTime.utc_now()
    {output, exit_code} = System.cmd(bp_binary(), ["run" | command_line_args])
    end_time = DateTime.utc_now()

    if exit_code == 0 do
      {:ok, DateTime.diff(end_time, start_time, :microsecond)}
    else
      {:error, {:build_pipeline_run_failed, output}}
    end
  end

  defp bp_binary do
    if Application.get_env(:build_pipeline, :env) == :test do
      "./bp"
    else
      "bp"
    end
  end
end
