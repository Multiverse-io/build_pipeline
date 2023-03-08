defmodule BuildPipeline.TerminalPrinter do
  alias IO.ANSI

  # TODO write tests
  def runner_update(messages, server_state) when is_list(messages) do
    Enum.each(messages, fn message -> runner_update(message, server_state) end)
  end

  def runner_update(%{line_update: true} = message, server_state) do
    %{ansi_prefix: ansi_prefix, prefix: prefix, suffix: suffix, runner_pid: runner_pid} = message
    %{terminal_line_number: line_number} = server_state[:runners][runner_pid]

    message = combine_onto_one_line(prefix, suffix, server_state.terminal_width)
    wrapped_message = "#{ansi_prefix}#{message}#{ANSI.reset()}"

    max_lines = max_runner_output_lines(server_state.runners)

    line_shift = max_lines - line_number + 1

    if should_print_runner_output?() do
      IO.write(
        "\r#{ANSI.cursor_up(line_shift)}\r#{ANSI.clear_line()}#{wrapped_message}#{ANSI.cursor_down(line_shift)}\r"
      )
    end
  end

  def runner_update(%{line_update: false, message: message}, _server_state) do
    if should_print_runner_output?() do
      IO.puts(message <> ANSI.reset())
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

  defp max_runner_output_lines(runners) do
    Enum.reduce(runners, 0, fn {_pid, %{terminal_line_number: line_number}}, acc ->
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
