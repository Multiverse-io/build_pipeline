defmodule BuildPipeline.FinalResult do
  alias BuildPipeline.FinalResult.Builder

  def write(%{save_result: true} = server_state, runner_pid, exit_code) do
    server_state
    |> Builder.build(runner_pid, exit_code)
    |> write_to_file!(server_state.cwd)
  end

  def write(%{save_result: false}, _, _) do
    :noop
  end

  defp write_to_file!(payload, cwd) do
    File.write!(file_path(cwd), payload)
  end

  defp file_path(cwd) do
    Path.join([cwd, "build_pipeline", "previous_run_result.json"])
  end
end
