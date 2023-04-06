defmodule BuildPipeline.Run.Statistics.PrettyOutput do
  alias BuildPipeline.Run.PrettyDurationMessage

  def generate(branches) do
    output =
      branches
      |> Enum.with_index(1)
      |> Enum.map(fn {branch, index} ->
        """
        Branch #{index} - #{PrettyDurationMessage.create(branch.duration_in_microseconds)}
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
      "#{prefix}── #{command} [#{PrettyDurationMessage.create(duration_in_microseconds)}]"
    end)
    |> Enum.join("\n")
  end
end
