defmodule BuildPipeline.Run.TerminalPrinterTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureIO
  alias BuildPipeline.Run.TerminalPrinter
  alias BuildPipeline.Run.Builders.ServerStateBuilder

  describe "runner_update/2" do
    test "given a line_update: true update, it is printed on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      message = %{
        ansi_prefix: "ansi_red_colour",
        prefix: "do stuff",
        suffix: "[Running]",
        runner_pid: "fake_pid",
        line_update: true
      }

      runners = %{"fake_pid" => %{terminal_line_number: 1}}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      output = capture_io(fn -> TerminalPrinter.runner_update(message, server_state) end)

      assert output =~ "\r\e[1A\r\e[2Kansi_red_colourdo stuff [Running]\e[0m\e[1B\r"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "given a line_update: false update, it is printed on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      message = %{message: "hi mom", line_update: false}

      runners = %{"fake_pid" => %{terminal_line_number: 1}}

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      output = capture_io(fn -> TerminalPrinter.runner_update(message, server_state) end)

      assert output =~ "hi mom\e[0m\n"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "given a list of line_update: true messages, they are printed on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      messages = [
        %{
          ansi_prefix: "ansi_red_colour",
          prefix: "do stuff",
          suffix: "[Running]",
          runner_pid: "fake_pid_1",
          line_update: true
        },
        %{
          ansi_prefix: "ansi_red_colour",
          prefix: "do other stuff",
          suffix: "[Running]",
          runner_pid: "fake_pid_2",
          line_update: true
        }
      ]

      runners = %{
        "fake_pid_1" => %{terminal_line_number: 1},
        "fake_pid_2" => %{terminal_line_number: 2}
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      output = capture_io(fn -> TerminalPrinter.runner_update(messages, server_state) end)

      assert output =~
               "\r\e[2A\r\e[2Kansi_red_colourdo stuff [Running]\e[0m\e[2B\r\r\e[1A\r\e[2Kansi_red_colourdo other stuff [Running]\e[0m\e[1B\r"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "given a list of line_update: false messages, they are printed on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      messages = [
        %{message: "cool message", line_update: false},
        %{message: "other cool message", line_update: false}
      ]

      runners = %{
        "fake_pid_1" => %{terminal_line_number: 1},
        "fake_pid_2" => %{terminal_line_number: 2}
      }

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_runners(runners)

      output = capture_io(fn -> TerminalPrinter.runner_update(messages, server_state) end)

      assert output =~ "cool message\e[0m\nother cool message\e[0m\n"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end
  end
end
