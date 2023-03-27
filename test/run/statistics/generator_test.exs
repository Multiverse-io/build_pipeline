defmodule BuildPipeline.Run.Statistics.GeneratorTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.Builders.RunnersBuilder
  alias BuildPipeline.Run.Statistics.Generator

  describe "dependency_tree_branches/1" do
    test "x" do
      a =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("a")
        |> RunnersBuilder.with_duration_in_microseconds(1)

      b =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("b")
        |> RunnersBuilder.with_duration_in_microseconds(2)
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      c =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("c")
        |> RunnersBuilder.with_duration_in_microseconds(4)
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      # TODO continue here dude
      assert Generator.dependency_tree_branches([a, b, c]) ==
               %{
                 tree: %{
                   "a" => %{"b" => nil, "c" => nil}
                 },
                 build_steps: %{
                   "a" => %{duration_in_microseconds: 1, status: :complete, exit_code: 0},
                   "b" => %{duration_in_microseconds: 2, status: :complete, exit_code: 0},
                   "c" => %{duration_in_microseconds: 4, status: :complete, exit_code: 0}
                 }
               }
    end
  end
end
