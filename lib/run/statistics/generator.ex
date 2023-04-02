defmodule BuildPipeline.Run.Statistics.Generator do
  alias BuildPipeline.Run.Statistics.Branches

  def generate(_runners, _show_stats? = false) do
    {:error, :not_showing_stats}
  end

  def generate(runners, _show_stats? = true) do
    if all_runners_completed?(runners) do
      runners = Enum.map(runners, fn {_pid, runner} -> runner end)
      runner_map = runner_map(runners)
      roots = roots(runners)
      deps = deps(runner_map)
      branches = Branches.branches(roots, deps)
      {:ok, stats_for_branches(branches, runner_map)}
    else
      {:error, :run_failed}
    end
  end

  # TODO pull this out into a separate module (& test it?)
  def prettify_output(branches) do
    output =
      branches
      |> Enum.with_index(1)
      |> Enum.map(fn {branch, index} ->
        """
        Branch #{index} - #{duration_message(branch.duration_in_microseconds)}
        #{branch_steps_output(branch.steps)}

        """
      end)
      |> Enum.join()

    footer = "******************"

    header = """

    #{footer}
    *** Statistics ***
    #{footer}

    """

    {:ok, header <> output <> footer}
  end

  defp branch_steps_output(steps) do
    [%{command: last} | _] = Enum.reverse(steps)

    steps
    |> Enum.map(fn %{command: command, duration_in_microseconds: duration_in_microseconds} ->
      prefix = if command == last, do: "└", else: "├"
      "#{prefix}── #{command} [#{duration_message(duration_in_microseconds)}]"
    end)
    |> Enum.join("\n")
  end

  defp all_runners_completed?(runners) do
    Enum.all?(runners, fn {_runner_pid, %{status: status}} ->
      status in [:skip, :complete]
    end)
  end

  defp stats_for_branches(branches, runner_map) do
    branches
    |> Enum.map(fn branch -> branch_stats(branch, runner_map) end)
    |> Enum.sort(&(&1.duration_in_microseconds >= &2.duration_in_microseconds))
  end

  defp branch_stats(steps, runner_map) do
    steps
    |> Enum.reduce(%{duration_in_microseconds: 0, steps: []}, fn step, stats ->
      step_stats = step_stats(runner_map, step)

      %{
        stats
        | duration_in_microseconds:
            stats.duration_in_microseconds + step_stats.duration_in_microseconds,
          steps: [step_stats | stats.steps]
      }
    end)
    |> Map.update!(:steps, &Enum.reverse/1)
  end

  defp step_stats(runner_map, step) do
    runner_map
    |> Map.fetch!(step)
    |> Map.take([:command, :duration_in_microseconds, :status, :exit_code])
    |> Map.put_new(:duration_in_microseconds, 0)
  end

  defp runner_map(runners) do
    Map.new(runners, fn runner -> Map.pop!(runner, :build_step_name) end)
  end

  defp roots(runners), do: roots([], runners)

  defp roots(acc, []) do
    acc
  end

  defp roots(acc, [runner | rest]) do
    if MapSet.size(runner.depends_on) == 0 do
      roots([runner.build_step_name | acc], rest)
    else
      roots(acc, rest)
    end
  end

  defp deps(runner_map) do
    steps = Enum.map(runner_map, fn {step, _} -> step end)
    acc = Map.new(steps, fn step -> {step, []} end)

    deps(acc, steps, runner_map)
  end

  defp deps(acc, [], _runner_map) do
    Map.new(acc, fn {step, deps} -> {step, Enum.reverse(deps)} end)
  end

  defp deps(acc, [step | rest], runner_map) do
    %{depends_on: depends_on} = Map.fetch!(runner_map, step)

    depends_on
    |> MapSet.to_list()
    |> Enum.reduce(acc, fn dependant, acc ->
      Map.update(acc, dependant, [step], &[step | &1])
    end)
    |> deps(rest, runner_map)
  end

  defp duration_message(duration_in_microseconds) do
    cond do
      duration_in_microseconds < 1000 ->
        "#{duration_in_microseconds} μs"

      duration_in_microseconds < 1_000_000 ->
        "#{round(duration_in_microseconds / 1000)} ms"

      duration_in_microseconds < 60_000_000 ->
        "#{Float.round(duration_in_microseconds / 1_000_000, 1)} s"

      true ->
        "#{Float.round(duration_in_microseconds / 60_000_000, 1)} min"
    end
  end
end
