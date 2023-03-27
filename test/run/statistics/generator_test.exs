defmodule BuildPipeline.Run.Statistics.GeneratorTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.Builders.RunnersBuilder
  alias BuildPipeline.Run.Statistics.Generator

  describe "dependency_tree_branches/1" do
    test "x" do
      a =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_build_step_name("a")
        |> RunnersBuilder.with_duration_in_microseconds(1)

      b =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_build_step_name("b")
        |> RunnersBuilder.with_duration_in_microseconds(2)
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      #TODO continue here dude
      assert Generator.dependency_tree_branches([a]) ==
               [
                 %{
                   build_steps: [
                     %{build_step_name: "a", duration_in_microseconds: 1, status: :complete},
                     %{build_step_name: "b", duration_in_microseconds: 2, status: :complete}
                   ],
                   duration_in_microseconds: 3
                 }
               ]
    end
  end
end
