defmodule BuildPipeline.Run.Statistics.PrettyOutputTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.Statistics.PrettyOutput

  describe "generate/1" do
    test "given some stats, returns a pretty string" do
      branches = [
        %{
          duration_in_microseconds: 103,
          steps: [
            %{command: "A", duration_in_microseconds: 1, status: :complete, exit_code: 0},
            %{command: "B", duration_in_microseconds: 2, status: :complete, exit_code: 0},
            %{command: "C", duration_in_microseconds: 4, status: :complete, exit_code: 0},
            %{command: "F", duration_in_microseconds: 32, status: :complete, exit_code: 0},
            %{command: "G", duration_in_microseconds: 64, status: :complete, exit_code: 0}
          ]
        },
        %{
          duration_in_microseconds: 7,
          steps: [
            %{command: "A", duration_in_microseconds: 1, status: :complete, exit_code: 0},
            %{command: "B", duration_in_microseconds: 2, status: :complete, exit_code: 0},
            %{command: "C", duration_in_microseconds: 4, status: :complete, exit_code: 0},
            %{command: "D", duration_in_microseconds: 8, status: :complete, exit_code: 0},
            %{command: "E", duration_in_microseconds: 16, status: :complete, exit_code: 0}
          ]
        }
      ]

      expected_output = """

      ******************
      *** Statistics ***
      ******************

      Branch 1 - 103 μs
      ├── A [1 μs]
      ├── B [2 μs]
      ├── C [4 μs]
      ├── F [32 μs]
      └── G [64 μs]

      Branch 2 - 7 μs
      ├── A [1 μs]
      ├── B [2 μs]
      ├── C [4 μs]
      ├── D [8 μs]
      └── E [16 μs]

      ******************
      """

      assert {:ok, expected_output} == PrettyOutput.generate(branches)
    end

    test "shows durations rounded wtih the proper unit [μs, ms, s, min]" do
      branches = [
        %{
          duration_in_microseconds: 123_103_579,
          steps: [
            %{command: "A", duration_in_microseconds: 123, status: :complete, exit_code: 0},
            %{command: "B", duration_in_microseconds: 123_456, status: :complete, exit_code: 0},
            %{command: "C", duration_in_microseconds: 2_490_000, status: :complete, exit_code: 0},
            %{
              command: "D",
              duration_in_microseconds: 120_490_000,
              status: :complete,
              exit_code: 0
            }
          ]
        }
      ]

      expected_output = """

      ******************
      *** Statistics ***
      ******************

      Branch 1 - 2.1 min
      ├── A [123 μs]
      ├── B [123 ms]
      ├── C [2.5 s]
      └── D [2.0 min]

      ******************
      """

      assert {:ok, expected_output} == PrettyOutput.generate(branches)
    end
  end
end
