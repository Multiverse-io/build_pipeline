defmodule BuildPipeline.Run.JsonReport.Generator do
  def generate(runners) do
    steps =
      runners
      |> Enum.map(fn {_pid, runner} -> runner end)
      |> Enum.sort_by(& &1.build_step_name)
      |> Enum.map(&format_runner/1)

    %{steps: steps}
  end

  defp format_runner(runner) do
    Map.take(runner, [:build_step_name, :exit_code, :duration_in_microseconds, :status])
  end
end
