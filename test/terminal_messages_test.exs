defmodule BuildPipeline.TerminalMessagesTest do
  use ExUnit.Case, async: false
  alias IO.ANSI
  alias BuildPipeline.Builders.ServerStateBuilder
  alias BuildPipeline.TerminalMessages

  describe "pending/2" do
    test "with mode = verbose returns runners with terminal line numbers & messages" do
      runners = %{
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

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:normal)

      expected_messages = [
        %{message: "#{ANSI.light_magenta()}echo hello [Pending]", line_update: false},
        %{message: "#{ANSI.light_magenta()}echo walk over [Pending]", line_update: false}
      ]

      assert TerminalMessages.pending(server_state) == expected_messages
    end

    test "with mode = verbose and a really small terminal, we truncate the message" do
      runners = %{
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

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:verbose)
        |> ServerStateBuilder.with_terminal_width(10)

      expected_messages = [
        %{message: "#{ANSI.light_magenta()}echo hello", line_update: false},
        %{message: "#{ANSI.light_magenta()}echo walk ", line_update: false}
      ]

      assert TerminalMessages.pending(server_state) == expected_messages
    end
  end

  describe "running/2 - with mode = normal" do
    test "given runners & a runner_pid, returns the message to print" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)
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

  describe "running/2 - with mode = verbose" do
    test "given runners & a runner_pid, returns the message to print" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:verbose)
      %{runners: runners} = server_state

      runner_pid = runners |> Map.keys() |> hd()

      %{command: command} = runners[runner_pid]

      assert TerminalMessages.running(server_state, runner_pid) == %{
               message: "#{ANSI.magenta()}#{command} [Running]",
               line_update: false
             }
    end
  end

  describe "running/2 - with mode = debug" do
    test "given runners & a runner_pid, returns the message to print" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:debug)
      %{runners: runners} = server_state

      runner_pid = runners |> Map.keys() |> hd()

      %{command: command} = runners[runner_pid]

      assert TerminalMessages.running(server_state, runner_pid) == %{
               message:
                 "\e[35m---------------------------------------------------------------------\n#{command} [Running]\n---------------------------------------------------------------------\e[0m\n",
               line_update: false
             }
    end
  end

  describe "succeeded/2 - with mode = verbose" do
    test "returns the success output in a big block" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:verbose)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               line_update: false,
               message:
                 "#{ANSI.green()}---------------------------------------------------------------------\necho hi [Succeeded in 123 μs] ✔\n\n#{ANSI.reset()}hi\n\n#{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}\n"
             }
    end
  end

  describe "succeeded/2 - with mode = debug" do
    test "puts the same result verbose mode does" do
      verbose_server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:verbose)

      debug_server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(verbose_server_state, runner, "fake_pid") ==
               TerminalMessages.succeeded(debug_server_state, runner, "fake_pid")
    end
  end

  describe "succeeded/2 - with mode = normal" do
    test "given runner result returns the success message in μs when duration < 1ms " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 123,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.green(),
               prefix: "echo hi",
               suffix: "[Succeeded in 123 μs] ✔ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result returns the success message in ms when 1ms < duration < 1s " do
      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 2211,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.green(),
               prefix: "echo hi",
               suffix: "[Succeeded in 2 ms] ✔ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result returns the success message in s when 1s < duration < 1min " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 1_390_000,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.green(),
               prefix: "echo hi",
               suffix: "[Succeeded in 1.4 s] ✔ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result returns the success message in min when duration > 1min " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 60_000_001,
        output: "hi\n"
      }

      assert TerminalMessages.succeeded(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.green(),
               prefix: "echo hi",
               suffix: "[Succeeded in 1.0 min] ✔ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end
  end

  describe "failed/2 - with mode = verbose" do
    test "returns the failed output in a big block" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:verbose)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               line_update: false,
               message:
                 "#{ANSI.red()}---------------------------------------------------------------------\necho hi [Failed in 123 μs] ✘\n\n#{ANSI.reset()}it failed\n\n#{ANSI.red()}---------------------------------------------------------------------#{ANSI.reset()}\n"
             }
    end
  end

  describe "failed/2 - with mode = debug" do
    test "returns only the failed command name and duraction" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:debug)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               message:
                 "\e[31m---------------------------------------------------------------------\necho hi [Failed in 123 μs] ✘\n\e[31m---------------------------------------------------------------------\e[0m\n",
               line_update: false
             }
    end
  end

  describe "failed/2 - with mode = normal" do
    test "given runner result, returns the failed message in μs when duration < 1ms " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "echo hi",
               suffix: "[Failed in 123 μs] ✘ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result, returns the failed message in ms when 1ms < duration < 1s " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 2211,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "echo hi",
               suffix: "[Failed in 2 ms] ✘ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result, returns the failed message in s when 1s < duration < 1min " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 1_390_000,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "echo hi",
               suffix: "[Failed in 1.4 s] ✘ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end

    test "given runner result, returns the failed message in min when duration > 1min " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 0,
        duration_in_microseconds: 60_000_001,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "echo hi",
               suffix: "[Failed in 1.0 min] ✘ ",
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
        |> ServerStateBuilder.with_mode(:normal)
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

    test "with mode = verbose, the message format is different with line_update false" do
      runners = %{
        "fake_pid_1" => %{command: "1", status: :complete},
        "fake_pid_2" => %{command: "2", status: :incomplete},
        "fake_pid_3" => %{command: "3", status: :incomplete},
        "fake_pid_4" => %{command: "4", status: :complete}
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:verbose)
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

    test "with mode = verbose and a terminal exactly as wide as the message, we don't truncate the message" do
      runners = %{
        "fake_pid_2" => %{
          build_step_name: "approachHuman",
          command: "1234",
          command_env_vars: [],
          command_type: :shell_command,
          depends_on: MapSet.new([]),
          order: 1,
          status: :incomplete,
          terminal_line_number: 2
        }
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:verbose)
        |> ServerStateBuilder.with_terminal_width(14)

      expected_messages = [
        %{message: "#{ANSI.light_magenta()}1234 [Pending]", line_update: false}
      ]

      assert TerminalMessages.pending(server_state) == expected_messages
    end

    test "with mode = debug, returns the same output as mode = verbose" do
      runners = %{
        "fake_pid_1" => %{command: "1", status: :complete},
        "fake_pid_2" => %{command: "2", status: :incomplete},
        "fake_pid_3" => %{command: "3", status: :incomplete},
        "fake_pid_4" => %{command: "4", status: :complete}
      }

      verbose_server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:verbose)
        |> ServerStateBuilder.with_runners(runners)

      debug_server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:debug)
        |> ServerStateBuilder.with_runners(runners)

      assert TerminalMessages.abort(verbose_server_state) ==
               TerminalMessages.abort(debug_server_state)
    end
  end

  describe "failed_output/1" do
    test "with mode = verbose" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:verbose)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 60_000_001,
        output: "it failed\n"
      }

      assert TerminalMessages.failed_output(server_state, runner) == []
    end

    test "with mode = debug" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:debug)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 60_000_001,
        output: "it failed\n"
      }

      assert TerminalMessages.failed_output(server_state, runner) == []
    end

    test "with mode = normal" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        duration_in_microseconds: 60_000_001,
        output: "it worked!\n"
      }

      assert TerminalMessages.failed_output(server_state, runner) ==
               %{
                 message: "it worked!\n",
                 line_update: false
               }
    end
  end
end
