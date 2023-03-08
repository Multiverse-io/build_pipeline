defmodule BuildPipeline.TerminalMessages do
  alias IO.ANSI

  # TODO move the line numbering out into the server. or at least separate runner updating out of here
  def runners_pending(runners) do
    runners =
      Map.new(runners, fn {pid, build_step} ->
        {pid, Map.put(build_step, :terminal_line_number, build_step.order + 1)}
      end)

    %{runners: runners, messages: pending(runners)}
  end

  def running(%{runners: runners}, runner_pid) do
    %{command: command} = Map.fetch!(runners, runner_pid)

    %{
      ansi_prefix: ANSI.magenta(),
      prefix: command,
      suffix: "[Running]",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def succeeded(%{verbose: false}, runner, runner_pid) do
    %{command: command, duration_in_microseconds: duration_in_microseconds} = runner

    %{
      ansi_prefix: ANSI.green(),
      prefix: command,
      suffix: "[Finished in #{duration_message(duration_in_microseconds)}] ✔ ",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def succeeded(%{verbose: true}, runner, _runner_pid) do
    %{command: command, duration_in_microseconds: duration_in_microseconds, output: output} =
      runner

    message = """
    #{ANSI.green()}---------------------------------------------------------------------
    #{command} [Finished in #{duration_message(duration_in_microseconds)}] ✔

    #{output}
    #{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}
    """

    %{line_update: false, message: message}
  end

  def failed(%{verbose: true} = _server_state, runner, _runner_pid) do
    %{command: command, duration_in_microseconds: duration_in_microseconds, output: output} =
      runner

    message = """
    #{ANSI.red()}---------------------------------------------------------------------
    #{command} [Finished in #{duration_message(duration_in_microseconds)}] ✘

    #{output}
    #{ANSI.red()}---------------------------------------------------------------------#{ANSI.reset()}
    """

    %{line_update: false, message: message}
  end

  def failed(%{verbose: false} = _server_state, runner, runner_pid) do
    %{command: command, duration_in_microseconds: duration_in_microseconds} = runner

    %{
      ansi_prefix: ANSI.red(),
      prefix: command,
      suffix: "[Finished in #{duration_message(duration_in_microseconds)}] ✘ ",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def abort(%{runners: runners} = _server_state) do
    runners
    |> Enum.reject(fn {_runner_pid, %{status: status}} -> status == :complete end)
    |> Enum.map(fn {runner_pid, %{command: command}} ->
      %{
        ansi_prefix: "#{ANSI.magenta()}#{ANSI.crossed_out()}",
        prefix: command,
        suffix: "[Aborted]",
        runner_pid: runner_pid,
        line_update: true
      }
    end)
  end

  # TODO sort verbose out
  # def print_succeeded(%{verbose: true} = server_state, result, runner_pid) do
  #  {finished_message, ansi_prefix} = runner_finished_in_duration_message(result)

  #  message = """
  #  #{ANSI.green()}---------------------------------------------------------------------
  #  #{finished_message}

  #  #{result.output}
  #  #{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}
  #  """

  #  print_update(server_state, runner_pid, message, ansi_prefix)
  # end
  # def print_failed(%{verbose: true} = server_state, result, runner_pid) do
  #  {message, ansi_prefix} = runner_failed_duration_message(result)

  #  message = """
  #  #{ansi_prefix}---------------------------------------------------------------------
  #  #{message}

  #  #{result.output}
  #  #{ansi_prefix}---------------------------------------------------------------------#{ANSI.reset()}
  #  """

  #  print_update(server_state, runner_pid, message, ansi_prefix)
  # end

  defp pending(runners) do
    runners
    |> Enum.sort(fn {_, %{order: order_1}}, {_, %{order: order_2}} -> order_1 <= order_2 end)
    |> Enum.map(fn {_pid, %{command: command}} ->
      %{ansi_prefix: ANSI.light_magenta(), prefix: command, suffix: "[Pending]"}
    end)
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
