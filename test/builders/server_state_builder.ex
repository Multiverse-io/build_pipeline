defmodule BuildPipeline.Builders.ServerStateBuilder do
  alias BuildPipeline.Builders.RunnersBuilder

  def build do
    runners = RunnersBuilder.build()

    %{
      runners: runners,
      parent_pid: self(),
      terminal_width: 200,
      verbose: false
    }
  end

  def with_verbose(server_state, verbose) do
    update_in(server_state, [:verbose], fn _ -> verbose end)
  end

  def with_runners(server_state, runners) do
    update_in(server_state, [:runners], fn _ -> runners end)
  end
end
