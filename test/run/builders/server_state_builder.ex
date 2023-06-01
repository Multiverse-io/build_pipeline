defmodule BuildPipeline.Run.Builders.ServerStateBuilder do
  alias BuildPipeline.Run.Builders.RunnersBuilder

  def build do
    runners = RunnersBuilder.build_many()

    %{
      runners: runners,
      parent_pid: self(),
      terminal_width: 200,
      mode: :normal,
      save_result: false,
      show_stats: false,
      json_report: false,
      cwd: "."
    }
  end

  def with_mode(server_state, mode) do
    update_in(server_state, [:mode], fn _ -> mode end)
  end

  def with_runners(server_state, runners) do
    update_in(server_state, [:runners], fn _ -> runners end)
  end

  def with_terminal_width(server_state, terminal_width) do
    update_in(server_state, [:terminal_width], fn _ -> terminal_width end)
  end

  def with_cwd(server_state, cwd) do
    update_in(server_state, [:cwd], fn _ -> cwd end)
  end

  def with_save_result(server_state, save_result) do
    update_in(server_state, [:save_result], fn _ -> save_result end)
  end

  def with_show_stats(server_state, show_stats) do
    update_in(server_state, [:show_stats], fn _ -> show_stats end)
  end

  def with_json_report(server_state, json_report) do
    update_in(server_state, [:json_report], fn _ -> json_report end)
  end
end
