defmodule BuildPipeline.FinalResult.Builder do
  alias BuildPipeline.Const

  @successful Const.successful()
  @failed Const.failed()
  @not_started Const.not_started()
  @skipped Const.skipped()

  def build(server_state, runner_pid, exit_code) do
    build_final_result(server_state.runners, runner_pid, exit_code)
  end

  defp build_final_result(runners, exiting_runner_pid, exiting_runner_exit_code) do
    runners
    |> Enum.sort_by(&(&1 |> elem(1) |> Map.fetch!(:order)), &<=/2)
    |> Enum.map(fn {runner_pid, runner} ->
      %{
        "buildStepName" => runner.build_step_name,
        "result" => result(runner_pid, runner, exiting_runner_pid, exiting_runner_exit_code)
      }
    end)
    |> Jason.encode!()
  end

  defp result(runner_pid, _runner, runner_pid, exit_code) do
    exit_code_to_result(exit_code)
  end

  defp result(_runner_pid, runner, _exiting_runner_pid, _exiting_runner_exit_code) do
    runner_result(runner)
  end

  defp exit_code_to_result(0), do: @successful
  defp exit_code_to_result(_), do: @failed

  defp runner_result(%{status: :complete, exit_code: exit_code}) do
    exit_code_to_result(exit_code)
  end

  defp runner_result(%{skip: true}) do
    @skipped
  end

  defp runner_result(%{status: :incomplete, skip: false}) do
    @not_started
  end
end
