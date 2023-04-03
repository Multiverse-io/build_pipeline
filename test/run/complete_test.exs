defmodule BuildPipeline.Run.CompleteTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.Complete
  alias BuildPipeline.Run.Builders.RunnersBuilder

  describe "complete?/1" do
    test "given runners that all have the status :complete, returns true" do
      runners = %{
        RunnersBuilder.pid() => RunnersBuilder.build_complete(),
        RunnersBuilder.pid() => RunnersBuilder.build_complete(),
        RunnersBuilder.pid() => RunnersBuilder.build_complete()
      }

      assert Complete.complete?(runners) == true
    end

    test "given runners that all have the status :skip, returns true" do
      runners = %{
        RunnersBuilder.pid() => skipped_runner(),
        RunnersBuilder.pid() => skipped_runner(),
        RunnersBuilder.pid() => skipped_runner()
      }

      assert Complete.complete?(runners) == true
    end

    test "given runners that all have either the status :skip or have completed, returns true" do
      runners = %{
        RunnersBuilder.pid() => skipped_runner(),
        RunnersBuilder.pid() => skipped_runner(),
        RunnersBuilder.pid() => RunnersBuilder.build_complete(),
        RunnersBuilder.pid() => skipped_runner(),
        RunnersBuilder.pid() => RunnersBuilder.build_complete()
      }

      assert Complete.complete?(runners) == true
    end

    test "given any runners that are neither complete nor skipped returns false" do
      runners = %{
        RunnersBuilder.pid() => skipped_runner(),
        RunnersBuilder.pid() => skipped_runner(),
        RunnersBuilder.pid() => RunnersBuilder.build_complete(),
        RunnersBuilder.pid() => skipped_runner(),
        RunnersBuilder.pid() => RunnersBuilder.build_incomplete(),
        RunnersBuilder.pid() => RunnersBuilder.build_complete()
      }

      assert Complete.complete?(runners) == false
    end
  end

  defp skipped_runner do
    RunnersBuilder.build_incomplete() |> RunnersBuilder.with_status_skip()
  end
end
