defmodule BuildPipeline.Run.Statistics do
  alias BuildPipeline.Run.Result
  alias BuildPipeline.Run.Statistics.Generator

  def print(state) do
    state.runners
    |> Generator.generate(state.show_stats)
    |> Result.and_then(&Generator.prettify_output/1)
    |> case do
      {:ok, msg} -> puts(msg)
      error -> error
    end
  end

  defp puts(msg) do
    if should_print_runner_output?() do
      IO.puts(msg)
    end
  end

  defp should_print_runner_output? do
    Application.get_env(:build_pipeline, :print_runner_output, true)
  end
end
