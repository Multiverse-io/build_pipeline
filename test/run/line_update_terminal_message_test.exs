defmodule BuildPipeline.Run.LineUpdateTerminalMessageTest do
  use ExUnit.Case, async: true
  alias IO.ANSI
  alias BuildPipeline.Run.Builders.ServerStateBuilder
  alias BuildPipeline.Run.LineUpdateTerminalMessage

  describe "build/2" do
    test "given a line_update: true message, builds it" do
      message = %{
        ansi_prefix: "coolAnsiPrefix",
        prefix: "echo 'hello mother'",
        suffix: "[Running]",
        runner_pid: "fake_pid",
        line_update: true
      }

      runners = %{"fake_pid" => %{terminal_line_number: 1}}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      assert LineUpdateTerminalMessage.build(message, server_state) ==
               "\r#{ANSI.cursor_up(1)}\r#{ANSI.clear_line()}coolAnsiPrefixecho 'hello mother' [Running]#{ANSI.reset()}#{ANSI.cursor_down(1)}\r"
    end

    test "given a line_update: false message, raises because we should never call it in this situation" do
      message = %{
        ansi_prefix: "coolAnsiPrefix",
        prefix: "echo 'hello mother'",
        suffix: "[Running]",
        runner_pid: "fake_pid",
        line_update: false
      }

      runners = %{"fake_pid" => %{terminal_line_number: 1}}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      assert_raise FunctionClauseError, fn ->
        LineUpdateTerminalMessage.build(message, server_state)
      end
    end

    test "when the terminal line number to update is large, we move the cursor accordingly" do
      message = %{
        ansi_prefix: "coolAnsiPrefix",
        prefix: "echo 'hello mother'",
        suffix: "[Running]",
        runner_pid: "fake_pid_1",
        line_update: true
      }

      runners = %{
        "fake_pid_1" => %{terminal_line_number: 1},
        "fake_pid_2" => %{terminal_line_number: 2},
        "fake_pid_3" => %{terminal_line_number: 3},
        "fake_pid_4" => %{terminal_line_number: 4},
        "fake_pid_5" => %{terminal_line_number: 5},
        "fake_pid_6" => %{terminal_line_number: 6},
        "fake_pid_7" => %{terminal_line_number: 7},
        "fake_pid_8" => %{terminal_line_number: 8},
        "fake_pid_9" => %{terminal_line_number: 9},
        "fake_pid_10" => %{terminal_line_number: 10}
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      assert LineUpdateTerminalMessage.build(message, server_state) ==
               "\r#{ANSI.cursor_up(10)}\r#{ANSI.clear_line()}coolAnsiPrefixecho 'hello mother' [Running]#{ANSI.reset()}#{ANSI.cursor_down(10)}\r"
    end

    test "moves the cursor the appropriate number of lines up and down depending on the line number of the runner PID's message" do
      runners = %{
        "fake_pid_1" => %{terminal_line_number: 1},
        "fake_pid_2" => %{terminal_line_number: 2},
        "fake_pid_3" => %{terminal_line_number: 3},
        "fake_pid_4" => %{terminal_line_number: 4},
        "fake_pid_5" => %{terminal_line_number: 5},
        "fake_pid_6" => %{terminal_line_number: 6},
        "fake_pid_7" => %{terminal_line_number: 7},
        "fake_pid_8" => %{terminal_line_number: 8},
        "fake_pid_9" => %{terminal_line_number: 9},
        "fake_pid_10" => %{terminal_line_number: 10}
      }

      message = %{
        ansi_prefix: "coolAnsiPrefix",
        prefix: "echo 'hello mother'",
        suffix: "[Running]",
        runner_pid: "fake_pid_1",
        line_update: true
      }

      tests = [
        %{pid: "fake_pid_1", line_shift: 10},
        %{pid: "fake_pid_2", line_shift: 9},
        %{pid: "fake_pid_3", line_shift: 8},
        %{pid: "fake_pid_4", line_shift: 7},
        %{pid: "fake_pid_5", line_shift: 6},
        %{pid: "fake_pid_6", line_shift: 5},
        %{pid: "fake_pid_7", line_shift: 4},
        %{pid: "fake_pid_8", line_shift: 3},
        %{pid: "fake_pid_9", line_shift: 2},
        %{pid: "fake_pid_10", line_shift: 1}
      ]

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      Enum.each(tests, fn %{pid: pid, line_shift: line_shift} ->
        message = Map.put(message, :runner_pid, pid)

        built_message = LineUpdateTerminalMessage.build(message, server_state)
        regex = ~r|\r\e\[#{line_shift}.*\e\[#{line_shift}B\r|
        assert Regex.match?(regex, built_message)
      end)
    end

    test "given a message who's prefix + suffix length > terminal width, the prefix gets truncated" do
      message = %{
        ansi_prefix: "YYY",
        prefix: "echo 'hello mother'",
        suffix: "[Running]",
        runner_pid: "fake_pid",
        line_update: true
      }

      runners = %{"fake_pid" => %{terminal_line_number: 1}}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_terminal_width(20)

      built_message = LineUpdateTerminalMessage.build(message, server_state)

      assert built_message =~ "YYYecho ' ... [Running]\e["
    end

    test "when the terminal is so small we can't render the suffix, we sack it off and render what we can from the start of the prefxx" do
      message = %{
        ansi_prefix: "YYY",
        prefix: "echo 'hello mother'",
        suffix: "[Running]",
        runner_pid: "fake_pid",
        line_update: true
      }

      runners = %{"fake_pid" => %{terminal_line_number: 1}}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_terminal_width(2)

      built_message = LineUpdateTerminalMessage.build(message, server_state)

      assert built_message =~ "YYYec\e["
    end

    test "if the truncated suffix length = the terminal width, just display chars from the prefix that fit" do
      message = %{
        ansi_prefix: "YYY",
        prefix: "echo 'hello mother'",
        suffix: "[Running]",
        runner_pid: "fake_pid",
        line_update: true
      }

      runners = %{"fake_pid" => %{terminal_line_number: 1}}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)
        |> ServerStateBuilder.with_terminal_width(14)

      built_message = LineUpdateTerminalMessage.build(message, server_state)

      assert built_message =~ "YYYecho 'hello mo\e["
    end
  end
end
