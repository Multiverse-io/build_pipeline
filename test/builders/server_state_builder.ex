defmodule BuildPipeline.Builders.ServerStateBuilder do
  alias BuildPipeline.Builders.RunnersBuilder

  def build do
    runners = RunnersBuilder.build()

    %{
      runners: runners,
      parent_pid: self(),
      runner_terminal_output: runner_terminal_output(runners),
      terminal_width: 200,
      verbose: false
    }
  end

  defp runner_terminal_output(runners) do
    runners
    |> Map.to_list()
    |> Enum.with_index(1)
    |> Map.new(fn {{pid, %{command: command}}, line_number} ->
      {pid, %{line_number: line_number, content: command}}
    end)
  end
end
