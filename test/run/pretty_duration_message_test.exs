defmodule BuildPipeline.Run.PrettyDurationMessageTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.PrettyDurationMessage

  describe "create/1 - given a duration_in_microseconds" do
    test "negative durations are given the proper units & rounding" do
      durations = [
        {-1, "-1 μs"},
        {-1000, "-1 ms"},
        {-1_000_000, "-1.0 s"},
        {-60_000_000, "-1.0 min"}
      ]

      Enum.each(durations, fn {duration, expected_output} ->
        result = PrettyDurationMessage.create(duration)

        assert result == expected_output,
               "Given input of #{duration} I expected a result of #{expected_output}, but instead this was returned: #{result}"
      end)
    end

    test "when it's less than 1k return a duration in microseconds [μs]" do
      durations = [
        {0, "0 μs"},
        {10, "10 μs"},
        {123, "123 μs"},
        {678, "678 μs"},
        {999, "999 μs"}
      ]

      Enum.each(durations, fn {duration, expected_output} ->
        result = PrettyDurationMessage.create(duration)

        assert result == expected_output,
               "Given input of #{duration} I expected a result of #{expected_output}, but instead this was returned: #{result}"
      end)
    end

    test "when it's less than 1M return a duration in microseconds [ms] rounded to the nearest whole number" do
      durations = [
        {1000, "1 ms"},
        {1499, "1 ms"},
        {1500, "2 ms"},
        {5100, "5 ms"},
        {123_499, "123 ms"},
        {123_500, "124 ms"},
        {999_999, "1000 ms"}
      ]

      Enum.each(durations, fn {duration, expected_output} ->
        result = PrettyDurationMessage.create(duration)

        assert result == expected_output,
               "Given input of #{duration} I expected a result of #{expected_output}, but instead this was returned: #{result}"
      end)
    end

    test "when it's less than 60M return a duration in seconds [s] rounded to one decimal place" do
      durations = [
        {1_000_000, "1.0 s"},
        {1_049_000, "1.0 s"},
        {1_050_000, "1.1 s"},
        {30_000_000, "30.0 s"},
        {59_999_999, "60.0 s"}
      ]

      Enum.each(durations, fn {duration, expected_output} ->
        result = PrettyDurationMessage.create(duration)

        assert result == expected_output,
               "Given input of #{duration} I expected a result of #{expected_output}, but instead this was returned: #{result}"
      end)
    end

    test "when it's greater than 60M return a duration in minutes [min] rounded to one decimal place" do
      durations = [
        {60_000_000, "1.0 min"},
        {62_999_999, "1.0 min"},
        {63_000_000, "1.1 min"},
        {120_000_000, "2.0 min"},
        {1_120_000_000, "18.7 min"},
        {9_999_999_999, "166.7 min"}
      ]

      Enum.each(durations, fn {duration, expected_output} ->
        result = PrettyDurationMessage.create(duration)

        assert result == expected_output,
               "Given input of #{duration} I expected a result of #{expected_output}, but instead this was returned: #{result}"
      end)
    end
  end
end
