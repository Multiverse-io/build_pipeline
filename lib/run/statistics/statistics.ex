defmodule BuildPipeline.Run.Statistics do
  alias BuildPipeline.Run.Result
  alias BuildPipeline.Run.Statistics.Generator

  def print(state) do
    state.runners
    |> Generator.dependency_tree_branches()
    |> Result.and_then(&Generator.prettify_output/1)
    |> case do
      {:ok, printable} -> IO.puts(printable)
      {:error, error} -> IO.puts(error)
    end
  end
end
