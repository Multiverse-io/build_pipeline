defmodule BuildPipeline.RunnerTerminalOutputTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureIO
  alias IO.ANSI
  alias BuildPipeline.RunnerTerminalOutput
  alias BuildPipeline.Builders.{RunnersBuilder, ServerStateBuilder, ServerSetupBuilder}

  describe "print_all_pending/2" do
    test "returns the printed output with line numbers" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      server_setup = ServerSetupBuilder.build().setup
      runners = RunnersBuilder.build()

      [
        {runner_pid_1, runner_1},
        {runner_pid_2, runner_2},
        {runner_pid_3, runner_3},
        {runner_pid_4, runner_4},
        {runner_pid_5, runner_5},
        {runner_pid_6, runner_6}
      ] = Map.to_list(runners)

      output =
        capture_io(fn ->
          assert %{
                   ^runner_pid_1 => %{line_number: 1, content: runner_1_content},
                   ^runner_pid_2 => %{line_number: 2, content: runner_2_content},
                   ^runner_pid_3 => %{line_number: 3, content: runner_3_content},
                   ^runner_pid_4 => %{line_number: 4, content: runner_4_content},
                   ^runner_pid_5 => %{line_number: 5, content: runner_5_content},
                   ^runner_pid_6 => %{line_number: 6, content: runner_6_content}
                 } = RunnerTerminalOutput.print_all_pending(runners, server_setup)

          assert runner_1_content =~ runner_1.command
          assert runner_2_content =~ runner_2.command
          assert runner_3_content =~ runner_3.command
          assert runner_4_content =~ runner_4.command
          assert runner_5_content =~ runner_5.command
          assert runner_6_content =~ runner_6.command
        end)

      assert output =~
               "#{ANSI.light_magenta()}#{runner_1.command} [Pending]#{ANSI.reset()}\n#{ANSI.light_magenta()}#{runner_2.command} [Pending]#{ANSI.reset()}\n#{ANSI.light_magenta()}#{runner_3.command} [Pending]#{ANSI.reset()}\n#{ANSI.light_magenta()}#{runner_4.command} [Pending]#{ANSI.reset()}\n#{ANSI.light_magenta()}#{runner_5.command} [Pending]#{ANSI.reset()}\n#{ANSI.light_magenta()}#{runner_6.command} [Pending]#{ANSI.reset()}\n"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end
  end

  describe "print_update/4" do
    test "given the server_state, a runner pid, a message and an ANSI prefix, prints the message and updates the server_state with the new message contents" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)
      server_state = ServerStateBuilder.build()

      [{runner_pid, %{command: command}} | _] = Map.to_list(server_state.runners)

      new_message = "cool message"
      ansi_prefix = ANSI.magenta()
      expected_new_content = command <> " " <> new_message

      output =
        capture_io(fn ->
          new_server_state =
            RunnerTerminalOutput.print_update(server_state, runner_pid, new_message, ansi_prefix)

          expected_server_state =
            put_in(
              server_state,
              [:runner_terminal_output, runner_pid, :content],
              expected_new_content
            )

          assert new_server_state[:runner_terminal_output][runner_pid][:content] ==
                   expected_new_content

          assert new_server_state == expected_server_state
        end)

      assert output =~
               "\r#{ANSI.cursor_up(6)}\r#{ANSI.clear_line()}#{ansi_prefix}#{expected_new_content}#{ANSI.reset()}#{ANSI.cursor_down(6)}\r"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end
  end
end
