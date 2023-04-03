defmodule BuildPipeline.Run.Statistics.PrettyOutput do
  def generate(branches) do
    output =
      branches
      |> Enum.with_index(1)
      |> Enum.map(fn {branch, index} ->
        """
        Branch #{index} - #{duration(branch.duration_in_microseconds)}
        #{branch_steps_output(branch.steps)}

        """
      end)
      |> Enum.join()

    footer = "******************"

    header = """

    #{footer}
    *** Statistics ***
    #{footer}

    """

    {:ok, header <> output <> footer <> "\n"}
  end

  defp branch_steps_output(steps) do
    [%{command: last} | _] = Enum.reverse(steps)

    steps
    |> Enum.map(fn %{command: command, duration_in_microseconds: duration_in_microseconds} ->
      prefix = if command == last, do: "└", else: "├"
      "#{prefix}── #{command} [#{duration(duration_in_microseconds)}]"
    end)
    |> Enum.join("\n")
  end

  defp duration(duration_in_microseconds) do
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
