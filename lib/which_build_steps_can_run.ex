defmodule BuildPipeline.WhichBuildStepsCanRun do
  # TODO rename module to reflect that it also returns the runner options
  def determine(%{runners: runners, mode: mode}) do
    runners
    |> concurrently_runable()
    |> modal_filter(mode)
    |> Enum.map(fn {pid, _} -> {pid, modal_runner_opts(mode)} end)
  end

  defp modal_filter(runners, :verbose) do
    runners
  end

  defp modal_filter(runners, :normal) do
    runners
  end

  defp modal_filter(runners, :debug) do
    runners
    |> Enum.sort_by(&runner_order/1, &<=/2)
    |> case do
      [first | _] -> [first]
      _ -> []
    end
  end

  defp modal_runner_opts(:normal), do: []
  defp modal_runner_opts(:verbose), do: []
  defp modal_runner_opts(:debug), do: [print_cmd_output: true]

  defp runner_order({_pid, %{order: order}}), do: order

  defp concurrently_runable(runners) do
    completed_runners = completed_runners_by_name(runners)

    Enum.filter(runners, fn {_, %{status: status, depends_on: depends_on}} ->
      status == :incomplete && MapSet.subset?(depends_on, completed_runners)
    end)
  end

  defp completed_runners_by_name(runners) do
    runners
    |> Enum.filter(fn {_, %{status: status}} -> status == :complete end)
    |> Enum.map(fn {_, %{build_step_name: build_step_name}} -> build_step_name end)
    |> MapSet.new()
  end
end
