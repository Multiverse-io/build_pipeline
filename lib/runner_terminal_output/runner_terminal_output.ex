defmodule BuildPipeline.RunnerTerminalOutput do
  alias IO.ANSI

  # TODO add tests for all the untested functions in here
  # TODO reconsider the abstration here? - separate IO.puts from output message building
  def print_all_pending(runners, %{terminal_width: terminal_width}) do
    runners
    |> Enum.map(fn {pid, build_step} -> {pid, build_step} end)
    |> Enum.sort(fn {_, %{order: order_1}}, {_, %{order: order_2}} -> order_1 <= order_2 end)
    |> Map.new(fn {pid, %{order: order, command: command}} ->
      line_number = order + 1

      message = combine_onto_one_line(command, "[Pending]", terminal_width)
      wrapped_message = "#{ANSI.light_magenta()}#{message}#{ANSI.reset()}"

      if should_print_runner_output?() do
        IO.puts(wrapped_message)
      end

      {pid, %{line_number: line_number, content: message}}
    end)
  end

  def print_update(%{verbose: false} = server_state, runner_pid, message, ansi_prefix) do
    command = server_state[:runners][runner_pid][:command]
    line_number = server_state[:runner_terminal_output][runner_pid][:line_number]

    message = combine_onto_one_line(command, message, server_state.terminal_width)
    wrapped_message = "#{ansi_prefix}#{message}#{ANSI.reset()}"

    max_lines = max_runner_output_lines(server_state.runner_terminal_output)

    line_shift = max_lines - line_number + 1

    if should_print_runner_output?() do
      IO.write(
        "\r#{ANSI.cursor_up(line_shift)}\r#{ANSI.clear_line()}#{wrapped_message}#{ANSI.cursor_down(line_shift)}\r"
      )
    end

    output_line = %{line_number: line_number, content: message}

    %{
      server_state
      | runner_terminal_output:
          Map.put(server_state.runner_terminal_output, runner_pid, output_line)
    }
  end

  # TODO this makes no sense since message is just the suffix now! It's nonsense! fix it!
  def print_update(%{verbose: true} = server_state, _runner_pid, message, _ansi_prefix) do
    if should_print_runner_output?() do
      IO.puts(message)
    end

    server_state
  end

  def print_failed(%{verbose: false} = server_state, result, runner_pid) do
    {message, ansi_prefix} = runner_failed_duration_message(result)
    print_update(server_state, runner_pid, message, ansi_prefix)
  end

  def print_failed(%{verbose: true} = server_state, result, runner_pid) do
    {message, ansi_prefix} = runner_failed_duration_message(result)

    message = """
    #{ansi_prefix}---------------------------------------------------------------------
    #{message}

    #{result.output}
    #{ansi_prefix}---------------------------------------------------------------------#{ANSI.reset()}
    """

    print_update(server_state, runner_pid, message, ansi_prefix)
  end

  def print_succeeded(%{verbose: false} = server_state, result, runner_pid) do
    {message, ansi_prefix} = runner_finished_in_duration_message(result)

    # print_runner_message(server_state, runner_pid, message)
    print_update(server_state, runner_pid, message, ansi_prefix)
  end

  def print_succeeded(%{verbose: true} = server_state, result, runner_pid) do
    {finished_message, ansi_prefix} = runner_finished_in_duration_message(result)

    message = """
    #{ANSI.green()}---------------------------------------------------------------------
    #{finished_message}

    #{result.output}
    #{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}
    """

    print_update(server_state, runner_pid, message, ansi_prefix)
  end

  def print_aborted(server_state) do
    Enum.reduce(server_state.runners, server_state, fn
      {_runner_pid, %{status: :complete}}, server_state ->
        server_state

      {runner_pid, _}, server_state ->
        print_update(
          server_state,
          runner_pid,
          "[Aborted]",
          "#{ANSI.magenta()}#{ANSI.crossed_out()}"
        )
    end)
  end

  defp runner_finished_in_duration_message(result) do
    %{duration_in_microseconds: duration_in_microseconds} = result
    duration = duration_message(duration_in_microseconds)
    {"[Finished in #{duration}] ✔ ", ANSI.green()}
  end

  defp runner_failed_duration_message(result) do
    %{duration_in_microseconds: duration_in_microseconds} = result
    duration = duration_message(duration_in_microseconds)
    {"[Finished in #{duration}] ✔ ", ANSI.red()}
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
        "#{round(duration_in_microseconds / 60_000_000)} min"
    end
  end

  defp combine_onto_one_line(prefix, suffix, max_length) do
    prefix_len = String.length(prefix)
    suffix_len = String.length(suffix)

    if prefix_len + suffix_len + 1 < max_length do
      prefix <> " " <> suffix
    else
      acc_single_line_message(prefix, suffix, max_length)
    end
  end

  defp acc_single_line_message(prefix, suffix, max_length) do
    prefix = String.graphemes(prefix)
    suffix = [" ", ".", ".", ".", " " | String.graphemes(suffix)]

    allowed_prefix_len = max_length - length(suffix)

    Enum.join(Enum.take(prefix, allowed_prefix_len) ++ suffix)
  end

  defp max_runner_output_lines(runner_terminal_output) do
    Enum.reduce(runner_terminal_output, 0, fn {_pid, %{line_number: line_number}}, acc ->
      if line_number >= acc do
        line_number
      else
        acc
      end
    end)
  end

  defp should_print_runner_output? do
    Application.get_env(:build_pipeline, :print_runner_output, true)
  end
end
