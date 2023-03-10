defmodule BuildPipeline.TerminalMessages do
  alias IO.ANSI

  def pending(%{runners: runners, terminal_width: terminal_width}) do
    runners
    |> Enum.sort(fn {_, %{order: order_1}}, {_, %{order: order_2}} -> order_1 <= order_2 end)
    |> Enum.map(fn {_pid, %{command: command}} ->
      message = "#{command} [Pending]"

      truncated_msg =
        if String.length(message) > terminal_width do
          String.slice(message, 0..(terminal_width - 1))
        else
          message
        end

      %{message: "#{ANSI.light_magenta()}#{truncated_msg}", line_update: false}
    end)
  end

  def running(%{verbose: false, runners: runners}, runner_pid) do
    %{command: command} = Map.fetch!(runners, runner_pid)

    %{
      ansi_prefix: ANSI.magenta(),
      prefix: command,
      suffix: "[Running]",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def running(%{verbose: true, runners: runners}, runner_pid) do
    %{command: command} = Map.fetch!(runners, runner_pid)

    %{
      message: "#{ANSI.magenta()}#{command} [Running]",
      line_update: false
    }
  end

  def succeeded(%{verbose: false}, runner, runner_pid) do
    %{command: command, duration_in_microseconds: duration_in_microseconds} = runner

    %{
      ansi_prefix: ANSI.green(),
      prefix: command,
      suffix: "[Succeeded in #{duration_message(duration_in_microseconds)}] ✔ ",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def succeeded(%{verbose: true}, runner, _runner_pid) do
    %{command: command, duration_in_microseconds: duration_in_microseconds, output: output} =
      runner

    message = """
    #{ANSI.green()}---------------------------------------------------------------------
    #{command} [Succeeded in #{duration_message(duration_in_microseconds)}] ✔

    #{ANSI.reset()}#{output}
    #{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}
    """

    %{line_update: false, message: message}
  end

  def failed(%{verbose: true} = _server_state, runner, _runner_pid) do
    %{command: command, duration_in_microseconds: duration_in_microseconds, output: output} =
      runner

    message = """
    #{ANSI.red()}---------------------------------------------------------------------
    #{command} [Failed in #{duration_message(duration_in_microseconds)}] ✘

    #{ANSI.reset()}#{output}
    #{ANSI.red()}---------------------------------------------------------------------#{ANSI.reset()}
    """

    %{line_update: false, message: message}
  end

  def failed(%{verbose: false} = _server_state, runner, runner_pid) do
    %{command: command, duration_in_microseconds: duration_in_microseconds} = runner

    %{
      ansi_prefix: ANSI.red(),
      prefix: command,
      suffix: "[Failed in #{duration_message(duration_in_microseconds)}] ✘ ",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def abort(%{verbose: verbose, runners: runners} = _server_state) do
    runners
    |> Enum.reject(fn {_runner_pid, %{status: status}} -> status == :complete end)
    |> Enum.map(fn {runner_pid, %{command: command}} ->
      if verbose do
        %{
          message: "#{ANSI.magenta()}#{ANSI.crossed_out()}#{command} [Aborted]",
          line_update: false
        }
      else
        %{
          ansi_prefix: "#{ANSI.magenta()}#{ANSI.crossed_out()}",
          prefix: command,
          suffix: "[Aborted]",
          runner_pid: runner_pid,
          line_update: true
        }
      end
    end)
  end

  def failed_output(%{verbose: true}, _runner) do
    []
  end

  def failed_output(%{verbose: false}, %{output: output}) do
    %{
      message: output,
      line_update: false
    }
  end

  defp duration_message(duration_in_microseconds) do
    cond do
      duration_in_microseconds < 1000 ->
        "#{duration_in_microseconds} μs"

      duration_in_microseconds < 1_000_000 ->
        "#{round(duration_in_microseconds / 1000)} ms"

      duration_in_microseconds < 60_000_000 ->
        "#{Float.round(duration_in_microseconds / 1_000_000, 1)} s"

      true ->
        "#{Float.round(duration_in_microseconds / 60_000_000, 1)} min"
    end
  end
end
