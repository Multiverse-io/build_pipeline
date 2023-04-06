defmodule BuildPipeline.Run.PrettyDurationMessage do
  def create(duration_in_microseconds) do
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
