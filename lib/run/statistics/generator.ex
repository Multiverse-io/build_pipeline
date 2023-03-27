defmodule BuildPipeline.Run.Statistics.Generator do
  @depends_on_nothing MapSet.new([])

  def dependency_tree_branches(runners) do
    branches([], 0, runners, runners)
    # Enum.reduce(runners, {[], 0}, fn {_pid, runner}, {tree, depth} ->
    #  %{
    #    command: command,
    #    status: status,
    #    exit_code: exit_code,
    #    depends_on: depends_on,
    #    duration_in_microseconds: duration_in_microseconds
    #  } = runner

    #  if depends_on == @depends_on_nothing do
    #  end
    # end)
  end

  def prettify_output(x) do
    {:ok, x}
  end

  defp branches(tree, depth, [runner | rest], all) do
    %{
      command: command,
      status: status,
      exit_code: exit_code,
      depends_on: depends_on,
      duration_in_microseconds: duration_in_microseconds
    } = runner

    if MapSet.size(depends_on) == depth do
    end
  end
end
