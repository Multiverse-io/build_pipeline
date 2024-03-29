defmodule BuildPipeline.Run.TerminalPrinter do
  alias IO.ANSI
  alias BuildPipeline.Run.LineUpdateTerminalMessage

  def runner_update(messages, server_state) when is_list(messages) do
    Enum.each(messages, fn message -> runner_update(message, server_state) end)
  end

  def runner_update(%{line_update: true} = message, server_state) do
    if should_print_runner_output?() do
      printable_msg = LineUpdateTerminalMessage.build(message, server_state)
      IO.write(printable_msg)
    end
  end

  def runner_update(%{line_update: false, message: message}, _server_state) do
    if should_print_runner_output?() do
      IO.puts(message <> ANSI.reset())
    end
  end

  def runner_update(%{truncate: true} = message, server_state) do
    if should_print_runner_output?() do
      printable_msg = LineUpdateTerminalMessage.build(message, server_state)
      IO.puts(printable_msg)
    end
  end

  defp should_print_runner_output? do
    Application.get_env(:build_pipeline, :print_runner_output, true)
  end
end
