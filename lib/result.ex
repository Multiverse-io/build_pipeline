defmodule BuildPipeline.Result do
  @moduledoc false
  def and_then({:ok, result}, fun), do: fun.(result)
  def and_then(:ok, fun), do: fun.()
  def and_then(other, _), do: other
end
