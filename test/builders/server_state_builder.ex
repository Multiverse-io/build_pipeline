defmodule BuildPipeline.Builders.ServerStateBuilder do
  alias BuildPipeline.Builders.RunnersBuilder

  def build do
    runners = RunnersBuilder.build()

    %{
      runners: runners,
      parent_pid: self(),
      terminal_width: 200,
      mode: :normal
    }
  end

  def with_mode(server_state, mode) do
    update_in(server_state, [:mode], fn _ -> mode end)
  end

  # def with_verbose(server_state, verbose) do
  # update_in(server_state, [:verbose], fn _ -> verbose end)
  # end

  def with_runners(server_state, runners) do
    update_in(server_state, [:runners], fn _ -> runners end)
  end

  def with_terminal_width(server_state, terminal_width) do
    update_in(server_state, [:terminal_width], fn _ -> terminal_width end)
  end
end
