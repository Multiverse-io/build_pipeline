defmodule BuildPipeline.Run.Statistics.Generator do
  def dependency_tree_branches(runners) do
    branches(%{}, %{}, 0, runners, runners)
  end

  def prettify_output(x) do
    {:ok, x}
  end

  defp branches(tree, build_steps, []) do
    %{tree: tree, build_steps: build_steps}
  end

  defp branches(tree, build_steps, depth, [runner | rest], runners) do
    %{
      command: command,
      status: status,
      exit_code: exit_code,
      depends_on: depends_on
    } = runner

    raise "no"
    # tree = Map.put(tree, command, %{})
    # build_steps = Map.put(build_steps, command, runner)

    # 'branches(tree, build_steps,
  end
end
