defmodule BuildPipeline.Run.Complete do
  def complete?(runners) do
    Enum.all?(runners, fn {_runner_pid, %{status: status}} ->
      status in [:skip, :complete]
    end)
  end
end
