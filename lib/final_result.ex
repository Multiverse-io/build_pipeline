defmodule BuildPipeline.FinalResult do
  # TODO test this at this level
  def write(server_state, runner_pid, exit_code) do
    server_state.runners
    |> build(runner_pid, exit_code)
    |> write_to_file!(server_state.cwd)
  end

  defp write_to_file!(payload, cwd) do
    File.write!(file_path(cwd), payload)
  end

  defp build(runners, runner_pid, exit_code) do
    runners
    |> Enum.map(fn {_pid, runner} ->

      %{
        "buildStepName" => runner.build_step_name,
        "result" => "ass"
      }
    end)
    |> Jason.encode!()
  end

  defp file_path(cwd) do
    Path.join([cwd, "build_pipeline", "previous_run_result.json"])
  end
end
