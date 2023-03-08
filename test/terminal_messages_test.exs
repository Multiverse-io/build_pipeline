defmodule BuildPipeline.TerminalMessagesTest do
  use ExUnit.Case, async: false
  alias IO.ANSI
  alias BuildPipeline.Builders.ServerStateBuilder
  alias BuildPipeline.TerminalMessages

  describe "runners_pending/2" do
    test "given runners, returns runners with terminal line numbers & messages" do
      initial_runners = %{
        "fake_pid_2" => %{
          build_step_name: "approachHuman",
          command: "echo walk over",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 1,
          status: :incomplete
        },
        "fake_pid_1" => %{
          build_step_name: "sayHello",
          command: "echo hello",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 0,
          status: :incomplete
        }
      }

      expected_runners = %{
        "fake_pid_2" => %{
          build_step_name: "approachHuman",
          command: "echo walk over",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 1,
          status: :incomplete,
          terminal_line_number: 2
        },
        "fake_pid_1" => %{
          build_step_name: "sayHello",
          command: "echo hello",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 0,
          status: :incomplete,
          terminal_line_number: 1
        }
      }

      expected_messages = [
        %{ansi_prefix: ANSI.light_magenta(), prefix: "echo hello", suffix: "[Pending]"},
        %{ansi_prefix: ANSI.light_magenta(), prefix: "echo walk over", suffix: "[Pending]"}
      ]

      assert %{runners: runners, messages: messages} =
               TerminalMessages.runners_pending(initial_runners)

      assert expected_runners == runners
      assert messages == expected_messages
    end
  end

  describe "running/2 - with verbose false" do
    test "given runners & a runner_pid, returns the message to print" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)
      %{runners: runners} = server_state

      runner_pid = runners |> Map.keys() |> hd()

      %{command: command} = runners[runner_pid]

      assert TerminalMessages.running(server_state, runner_pid) == %{
               ansi_prefix: ANSI.magenta(),
               prefix: command,
               suffix: "[Running]",
               line_update: true,
               runner_pid: runner_pid
             }
    end
  end

  describe "running/2 - with verbose true" do
    test "given runners & a runner_pid, returns the message to print" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(true)
      %{runners: runners} = server_state

      runner_pid = runners |> Map.keys() |> hd()

      %{command: command} = runners[runner_pid]

      assert TerminalMessages.running(server_state, runner_pid) == %{
               message: "#{ANSI.magenta()}#{command} [Running]",
               line_update: false
             }
    end
  end

  describe "succeeded/2 - with verbose true" do
    test "returns the failed output in a big block" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(true)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               line_update: false,
               message:
                 "#{ANSI.green()}---------------------------------------------------------------------\necho hi [Finished in 123 μs] ✔\n\nhi\n\n#{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}\n"
             }
    end
  end

  describe "succeeded/2 - with verbose false" do
    test "given runner result returns the success message in μs when duration < 1ms " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 123,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.green(),
               prefix: "echo hi",
               suffix: "[Finished in 123 μs] ✔ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result returns the success message in ms when 1ms < duration < 1s " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 2211,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.green(),
               prefix: "echo hi",
               suffix: "[Finished in 2 ms] ✔ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result returns the success message in s when 1s < duration < 1min " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 1_390_000,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.green(),
               prefix: "echo hi",
               suffix: "[Finished in 1.4 s] ✔ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result returns the success message in min when duration > 1min " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 60_000_001,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.green(),
               prefix: "echo hi",
               suffix: "[Finished in 1.0 min] ✔ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end
  end

  describe "failed/2 - with verbose true" do
    test "returns the failed output in a big block" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(true)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               line_update: false,
               message:
                 "#{ANSI.red()}---------------------------------------------------------------------\necho hi [Finished in 123 μs] ✘\n\nit failed\n\n#{ANSI.red()}---------------------------------------------------------------------#{ANSI.reset()}\n"
             }
    end
  end

  describe "failed/2 - with verbose false" do
    test "given runner result, returns the failed message in μs when duration < 1ms " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "echo hi",
               suffix: "[Finished in 123 μs] ✘ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result, returns the failed message in ms when 1ms < duration < 1s " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 2211,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "echo hi",
               suffix: "[Finished in 2 ms] ✘ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result, returns the failed message in s when 1s < duration < 1min " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 1_390_000,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "echo hi",
               suffix: "[Finished in 1.4 s] ✘ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result, returns the failed message in min when duration > 1min " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_verbose(false)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 60_000_001,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "echo hi",
               suffix: "[Finished in 1.0 min] ✘ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end
  end

  describe "aborted/1" do
    test "incomplete runners return the abort message" do
      runners = %{
        "fake_pid_1" => %{command: "1", status: :complete},
        "fake_pid_2" => %{command: "2", status: :incomplete},
        "fake_pid_3" => %{command: "3", status: :incomplete},
        "fake_pid_4" => %{command: "4", status: :complete}
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_verbose(false)
        |> ServerStateBuilder.with_runners(runners)

      assert TerminalMessages.abort(server_state) == [
               %{
                 ansi_prefix: "#{ANSI.magenta()}#{ANSI.crossed_out()}",
                 prefix: "2",
                 suffix: "[Aborted]",
                 runner_pid: "fake_pid_2",
                 line_update: true
               },
               %{
                 ansi_prefix: "#{ANSI.magenta()}#{ANSI.crossed_out()}",
                 prefix: "3",
                 suffix: "[Aborted]",
                 runner_pid: "fake_pid_3",
                 line_update: true
               }
             ]
    end

    test "with verbose true, the message format is different with line_update false" do
      runners = %{
        "fake_pid_1" => %{command: "1", status: :complete},
        "fake_pid_2" => %{command: "2", status: :incomplete},
        "fake_pid_3" => %{command: "3", status: :incomplete},
        "fake_pid_4" => %{command: "4", status: :complete}
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_verbose(true)
        |> ServerStateBuilder.with_runners(runners)

      assert TerminalMessages.abort(server_state) == [
               %{
                 message: "#{ANSI.magenta()}#{ANSI.crossed_out()}2 [Aborted]",
                 line_update: false
               },
               %{
                 message: "#{ANSI.magenta()}#{ANSI.crossed_out()}3 [Aborted]",
                 line_update: false
               }
             ]
    end
  end
end
