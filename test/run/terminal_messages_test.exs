defmodule BuildPipeline.Run.TerminalMessagesTest do
  use ExUnit.Case, async: false
  alias IO.ANSI
  alias BuildPipeline.Run.Builders.{RunnersBuilder, ServerStateBuilder}
  alias BuildPipeline.Run.TerminalMessages

  describe "pending/2" do
    test "with mode = normal returns runners with terminal line numbers & messages" do
      pid_1 = RunnersBuilder.pid()

      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("echo walk over")
        |> RunnersBuilder.with_order(0)

      pid_2 = RunnersBuilder.pid()

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("echo hello")
        |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}])
        |> RunnersBuilder.with_order(1)

      runners = %{pid_1 => runner_1, pid_2 => runner_2}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:normal)

      expected_messages = [
        %{
          ansi_prefix: ANSI.light_magenta(),
          prefix: "echo walk over",
          suffix: "[Pending]",
          truncate: true
        },
        %{
          ansi_prefix: ANSI.light_magenta(),
          prefix: "MIX_ENV=test COOL=ENV echo hello",
          suffix: "[Pending]",
          truncate: true
        }
      ]

      assert TerminalMessages.pending(server_state) == expected_messages
    end

    test "if any steps have skip = true, don't say pending, say skipped" do
      pid_1 = RunnersBuilder.pid()

      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("echo 'hi mom'")
        |> RunnersBuilder.with_order(1)

      pid_2 = RunnersBuilder.pid()

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_status_skip()
        |> RunnersBuilder.with_command("echo 'walk over to mom'")
        |> RunnersBuilder.with_order(0)

      runners = %{pid_1 => runner_1, pid_2 => runner_2}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:normal)

      expected_messages = [
        %{
          ansi_prefix: "#{ANSI.green_background()}#{ANSI.black()}",
          prefix: "echo 'walk over to mom'",
          suffix: "[Skipped]",
          truncate: true
        },
        %{
          ansi_prefix: ANSI.light_magenta(),
          prefix: "echo 'hi mom'",
          suffix: "[Pending]",
          truncate: true
        }
      ]

      assert TerminalMessages.pending(server_state) == expected_messages
    end
  end

  describe "running/2 - with mode = normal" do
    test "given runners & a runner_pid, returns the message to print" do
      pid_1 = RunnersBuilder.pid()

      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("echo 'hi mom'")
        |> RunnersBuilder.with_order(1)

      pid_2 = RunnersBuilder.pid()

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_status_skip()
        |> RunnersBuilder.with_command("echo 'walk over to mom'")
        |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}])
        |> RunnersBuilder.with_order(0)

      runners = %{pid_1 => runner_1, pid_2 => runner_2}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:normal)

      expected_message_1 = %{
        ansi_prefix: ANSI.magenta(),
        prefix: "echo 'hi mom'",
        suffix: "[Running]",
        runner_pid: pid_1,
        line_update: true
      }

      expected_message_2 = %{
        ansi_prefix: ANSI.magenta(),
        prefix: "MIX_ENV=test COOL=ENV echo 'walk over to mom'",
        suffix: "[Running]",
        runner_pid: pid_2,
        line_update: true
      }

      assert TerminalMessages.running(server_state, pid_1) == expected_message_1
      assert TerminalMessages.running(server_state, pid_2) == expected_message_2
    end
  end

  describe "running/2 - with mode = verbose" do
    test "given runners & a runner_pid, returns the message to print" do
      pid_1 = RunnersBuilder.pid()

      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("echo 'hi mom'")
        |> RunnersBuilder.with_order(1)

      pid_2 = RunnersBuilder.pid()

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_status_skip()
        |> RunnersBuilder.with_command("echo 'walk over to mom'")
        |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}])
        |> RunnersBuilder.with_order(0)

      runners = %{pid_1 => runner_1, pid_2 => runner_2}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:verbose)

      expected_message_1 = %{
        message: "#{ANSI.magenta()}echo 'hi mom' [Running]",
        line_update: false
      }

      expected_message_2 = %{
        message: "#{ANSI.magenta()}MIX_ENV=test COOL=ENV echo 'walk over to mom' [Running]",
        line_update: false
      }

      assert TerminalMessages.running(server_state, pid_1) == expected_message_1
      assert TerminalMessages.running(server_state, pid_2) == expected_message_2
    end
  end

  describe "running/2 - with mode = debug" do
    test "given runners & a runner_pid, returns the message to print" do
      pid_1 = RunnersBuilder.pid()

      runner_1 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("echo 'hi mom'")
        |> RunnersBuilder.with_order(1)

      pid_2 = RunnersBuilder.pid()

      runner_2 =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_status_skip()
        |> RunnersBuilder.with_command("echo 'walk over to mom'")
        |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}])
        |> RunnersBuilder.with_order(0)

      runners = %{pid_1 => runner_1, pid_2 => runner_2}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:debug)

      expected_message_1 = %{
        message:
          "\e[35m---------------------------------------------------------------------\necho 'hi mom' [Running]\n---------------------------------------------------------------------\e[0m\n",
        line_update: false
      }

      expected_message_2 = %{
        message:
          "\e[35m---------------------------------------------------------------------\nMIX_ENV=test COOL=ENV echo 'walk over to mom' [Running]\n---------------------------------------------------------------------\e[0m\n",
        line_update: false
      }

      assert TerminalMessages.running(server_state, pid_1) == expected_message_1
      assert TerminalMessages.running(server_state, pid_2) == expected_message_2
    end
  end

  describe "succeeded/2 - with mode = verbose" do
    test "returns the success output in a big block" do
      pid_1 = RunnersBuilder.pid()

      runner_1 =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("echo 'hi mom'")
        |> RunnersBuilder.with_output("hi mom\n")
        |> RunnersBuilder.with_duration_in_microseconds(123)
        |> RunnersBuilder.with_order(1)

      pid_2 = RunnersBuilder.pid()

      runner_2 =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_status_skip()
        |> RunnersBuilder.with_command("echo 'walk over to mom'")
        |> RunnersBuilder.with_output("walk over to mom\n")
        |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}])
        |> RunnersBuilder.with_duration_in_microseconds(456)
        |> RunnersBuilder.with_order(0)

      runners = %{pid_1 => runner_1, pid_2 => runner_2}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:verbose)

      expected_message_1 = %{
        message:
          "#{ANSI.green()}---------------------------------------------------------------------\necho 'hi mom' [Succeeded in 123 μs] ✔\n\n#{ANSI.reset()}hi mom\n\n#{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}\n",
        line_update: false
      }

      expected_message_2 = %{
        message:
          "#{ANSI.green()}---------------------------------------------------------------------\nMIX_ENV=test COOL=ENV echo 'walk over to mom' [Succeeded in 456 μs] ✔\n\n#{ANSI.reset()}walk over to mom\n\n#{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}\n",
        line_update: false
      }

      assert TerminalMessages.succeeded(server_state, runner_1, pid_1) == expected_message_1
      assert TerminalMessages.succeeded(server_state, runner_2, pid_2) == expected_message_2
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
        command_env_vars: [],
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
      pid_1 = RunnersBuilder.pid()

      runner_1 =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("echo 'hi mom'")
        |> RunnersBuilder.with_duration_in_microseconds(123)
        |> RunnersBuilder.with_order(1)

      pid_2 = RunnersBuilder.pid()

      runner_2 =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_status_skip()
        |> RunnersBuilder.with_command("echo 'walk over to mom'")
        |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}])
        |> RunnersBuilder.with_duration_in_microseconds(456)
        |> RunnersBuilder.with_order(0)

      runners = %{pid_1 => runner_1, pid_2 => runner_2}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_mode(:normal)

      expected_message_1 = %{
        ansi_prefix: ANSI.green(),
        prefix: "echo 'hi mom'",
        suffix: "[Succeeded in 123 μs] ✔ ",
        line_update: true,
        runner_pid: pid_1
      }

      expected_message_2 = %{
        ansi_prefix: ANSI.green(),
        prefix: "MIX_ENV=test COOL=ENV echo 'walk over to mom'",
        suffix: "[Succeeded in 456 μs] ✔ ",
        line_update: true,
        runner_pid: pid_2
      }

      assert TerminalMessages.succeeded(server_state, runner_1, pid_1) == expected_message_1
      assert TerminalMessages.succeeded(server_state, runner_2, pid_2) == expected_message_2
    end
  end

  describe "failed/2 - with mode = verbose" do
    test "returns the failed output in a big block" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:verbose)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        command_env_vars: [{"MIX_ENV", "test"}, {"COOL", "ENV"}],
        duration_in_microseconds: 123,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               line_update: false,
               message:
                 "#{ANSI.red()}---------------------------------------------------------------------\nMIX_ENV=test COOL=ENV echo hi [Failed in 123 μs] ✘\n\n#{ANSI.reset()}it failed\n\n#{ANSI.red()}---------------------------------------------------------------------#{ANSI.reset()}\n"
             }
    end
  end

  describe "failed/2 - with mode = debug" do
    test "returns only the failed command name and duraction" do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:debug)

      runner = %{
        command: "echo hi",
        exit_code: 1,
        command_env_vars: [{"MIX_ENV", "test"}, {"COOL", "ENV"}],
        duration_in_microseconds: 123,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               message:
                 "\e[31m---------------------------------------------------------------------\nMIX_ENV=test COOL=ENV echo hi [Failed in 123 μs] ✘\n\e[31m---------------------------------------------------------------------\e[0m\n",
               line_update: false
             }
    end
  end

  describe "failed/2 - with mode = normal" do
    test "given runner result, returns the failed message in μs when duration < 1ms " do
      server_state = ServerStateBuilder.build() |> ServerStateBuilder.with_mode(:normal)

      runner = %{
        command: "echo hi",
        command_env_vars: [{"MIX_ENV", "test"}, {"COOL", "ENV"}],
        exit_code: 1,
        duration_in_microseconds: 123,
        output: "it failed\n"
      }

      assert TerminalMessages.failed(server_state, runner, "fake_pid") == %{
               ansi_prefix: ANSI.red(),
               prefix: "MIX_ENV=test COOL=ENV echo hi",
               suffix: "[Failed in 123 μs] ✘ ",
               line_update: true,
               runner_pid: "fake_pid"
             }
    end
  end

  describe "aborted/1" do
    test "incomplete runners return the abort message" do
      runners = %{
        "fake_pid_1" => RunnersBuilder.build_complete() |> RunnersBuilder.with_command("1"),
        "fake_pid_2" => RunnersBuilder.build_incomplete() |> RunnersBuilder.with_command("2"),
        "fake_pid_3" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_command("3")
          |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}]),
        "fake_pid_4" => RunnersBuilder.build_complete() |> RunnersBuilder.with_command("4")
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
                 prefix: "MIX_ENV=test COOL=ENV 3",
                 suffix: "[Aborted]",
                 runner_pid: "fake_pid_3",
                 line_update: true
               }
             ]
    end

    test "skipped runners don't come up as aborted" do
      runners = %{
        "fake_pid_1" => RunnersBuilder.build_incomplete() |> RunnersBuilder.with_status_skip(),
        "fake_pid_2" => RunnersBuilder.build_incomplete() |> RunnersBuilder.with_status_skip(),
        "fake_pid_3" => RunnersBuilder.build_incomplete(),
        "fake_pid_4" => RunnersBuilder.build_complete()
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_mode(:normal)
        |> ServerStateBuilder.with_runners(runners)

      assert [%{runner_pid: "fake_pid_3"}] = TerminalMessages.abort(server_state)
    end

    test "with mode = verbose, the message format is different with line_update false" do
      runners = %{
        "fake_pid_1" => RunnersBuilder.build_complete() |> RunnersBuilder.with_command("1"),
        "fake_pid_2" => RunnersBuilder.build_incomplete() |> RunnersBuilder.with_command("2"),
        "fake_pid_3" =>
          RunnersBuilder.build_incomplete()
          |> RunnersBuilder.with_command("3")
          |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}]),
        "fake_pid_4" => RunnersBuilder.build_complete() |> RunnersBuilder.with_command("4")
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
                 message:
                   "#{ANSI.magenta()}#{ANSI.crossed_out()}MIX_ENV=test COOL=ENV 3 [Aborted]",
                 line_update: false
               }
             ]
    end

    test "with mode = debug, returns the same output as mode = verbose" do
      runners = %{
        "fake_pid_1" => RunnersBuilder.build_complete() |> RunnersBuilder.with_command("1"),
        "fake_pid_2" => RunnersBuilder.build_incomplete() |> RunnersBuilder.with_command("2"),
        "fake_pid_3" => RunnersBuilder.build_incomplete() |> RunnersBuilder.with_command("3"),
        "fake_pid_4" => RunnersBuilder.build_complete() |> RunnersBuilder.with_command("4")
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
