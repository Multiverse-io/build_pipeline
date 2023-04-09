defmodule BuildPipeline.Run.LineUpdateTerminalMessage do
  alias IO.ANSI

  def build(%{line_update: true} = message, server_state) do
    %{ansi_prefix: ansi_prefix, prefix: prefix, suffix: suffix, runner_pid: runner_pid} = message
    %{runners: runners, terminal_width: terminal_width} = server_state
    %{terminal_line_number: line_number} = Map.fetch!(runners, runner_pid)

    message = single_line_message(prefix, suffix, terminal_width)
    wrapped_message = "#{ansi_prefix}#{message}#{ANSI.reset()}"

    max_lines = max_runner_output_lines(runners)

    line_shift = max_lines - line_number + 1

    "\r#{ANSI.cursor_up(line_shift)}\r#{ANSI.clear_line()}#{wrapped_message}#{ANSI.cursor_down(line_shift)}\r"
  end

  def build(%{truncate: true} = message, server_state) do
    %{ansi_prefix: ansi_prefix, prefix: prefix, suffix: suffix} = message
    %{terminal_width: terminal_width} = server_state

    message = single_line_message(prefix, suffix, terminal_width)
    "#{ansi_prefix}#{message}#{ANSI.reset()}"
  end

  defp single_line_message(prefix, suffix, max_length) do
    prefix_len = String.length(prefix)
    suffix_len = String.length(suffix)

    if prefix_len + suffix_len + 1 < max_length do
      prefix <> " " <> suffix
    else
      truncated_single_line_message(prefix, suffix, max_length)
    end
  end

  defp truncated_single_line_message(prefix, suffix, max_length) do
    prefix = String.graphemes(prefix)
    suffix = [" ", ".", ".", ".", " " | String.graphemes(suffix)]
    truncated_suffix_len = length(suffix)

    allowed_prefix_len = max_length - length(suffix)

    if truncated_suffix_len >= max_length do
      Enum.take(prefix, max_length)
    else
      Enum.join(Enum.take(prefix, allowed_prefix_len) ++ suffix)
    end
  end

  defp max_runner_output_lines(runners) do
    Enum.reduce(runners, 0, fn {_pid, %{terminal_line_number: line_number}}, acc ->
      if line_number >= acc do
        line_number
      else
        acc
      end
    end)
  end
end
