defmodule BuildPipeline.Run.Statistics.Generator do
  alias BuildPipeline.Run.Statistics.Branches

  def generate(runners) do
    runner_map = runner_map(runners)
    roots = roots(runners)

    deps =
      deps(runners, runner_map)
      |> IO.inspect()

    branches =
      Branches.branches(roots, deps)
      |> IO.inspect()

    # |> List.flatten()

    # |> IO.inspect()

    # generate(branches, runner_map)
  end

  def prettify_output(x) do
    {:ok, x}
  end

  defp generate(branches, runner_map) do
  end

  # defp generate(branches, runner_map) do
  #  generate(branches, runner_map, [])
  # end

  # defp generate([], _runner_map, acc) do
  #  acc
  # end

  # defp generate([{step, offshoots} | rest], runner_map, acc) do
  #  runner = step
  #  IO.inspect(step, label: "step")

  #  new =
  #    case offshoots do
  #      [] -> [runner]
  #      _ -> Enum.map(offshoots, fn offshoot -> [runner | generate([offshoot], runner_map)] end)
  #    end
  #    |> IO.inspect(label: "offshoots")

  #  generate(rest, runner_map, acc ++ new)
  # end

  defp fetch_runner_stats(runner_map, step) do
    Map.take(runner_map[step], [:command, :duration_in_microseconds, :status, :exit_code])
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

  defp deps(runners, runner_map), do: deps(%{}, runners, runner_map)

  defp deps(deps, [], _runner_map) do
    deps
  end

  defp deps(deps, [runner | rest], runner_map) do
    %{build_step_name: name, depends_on: depends_on} = runner

    depends_on
    |> MapSet.to_list()
    |> Enum.reduce(deps, fn dependant, deps ->
      Map.update(deps, dependant, [name], &[name | &1])
    end)
    |> deps(rest, runner_map)
  end
end
