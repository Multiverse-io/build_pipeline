defmodule BuildPipeline.Run.PrettyDurationMessage do
  def create(duration_in_microseconds) do
    {sign, duration_in_microseconds} = modulus(duration_in_microseconds)

    cond do
      duration_in_microseconds < 1000 ->
        "#{sign}#{duration_in_microseconds} μs"

      duration_in_microseconds < 1_000_000 ->
        "#{sign}#{round(duration_in_microseconds / 1000)} ms"

      duration_in_microseconds < 60_000_000 ->
        "#{sign}#{Float.round(duration_in_microseconds / 1_000_000, 1)} s"

      true ->
        "#{sign}#{Float.round(duration_in_microseconds / 60_000_000, 1)} min"
    end
  end

  defp modulus(duration_in_microseconds) do
    if duration_in_microseconds < 0 do
      {"-", -duration_in_microseconds}
    else
      {"", duration_in_microseconds}
    end
  end
end
