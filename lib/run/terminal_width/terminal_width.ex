defmodule BuildPipeline.Run.TerminalWidth do
  alias BuildPipeline.Run.TerminalWidth.TputCols

  def append_to_setup(config) do
    case determine() do
      {:ok, terminal_width} -> {:ok, put_in(config, [:setup, :terminal_width], terminal_width)}
      error -> error
    end
  end

  defp determine do
    case run_tput_cols() do
      %ErlangError{original: :enoent} ->
        {:error, {:terminal_width, :tput_not_on_system}}

      {result, 0} ->
        parse_tput_cols_output(result)

      {error, _exit_code} ->
        {:error, {:terminal_width, :unexpected_tput_result, error}}
    end
  end

  defp run_tput_cols do
    try do
      TputCols.run()
    rescue
      e in ErlangError -> e
    end
  end

  defp parse_tput_cols_output(result) do
    case result |> String.trim() |> Integer.parse() do
      {width, ""} ->
        {:ok, width}

      _ ->
        {:error, {:terminal_width, :unexpected_tput_result, result}}
    end
  end
end
