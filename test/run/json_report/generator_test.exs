defmodule BuildPipeline.Run.JsonReport.GeneratorTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.Builders.RunnersBuilder
  alias BuildPipeline.Run.JsonReport.Generator

  describe "generate/1" do
    test "with completed runs" do
      a =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("ac")
        |> RunnersBuilder.with_build_step_name("a")
        |> RunnersBuilder.with_duration_in_microseconds(1111)
        |> RunnersBuilder.with_exit_code(0)

      b =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("dc")
        |> RunnersBuilder.with_build_step_name("b")
        |> RunnersBuilder.with_duration_in_microseconds(2222)
        |> RunnersBuilder.with_exit_code(0)

      assert Generator.generate(%{
               RunnersBuilder.pid() => a,
               RunnersBuilder.pid() => b
             }) ==
               %{
                 steps: [
                   %{
                     build_step_name: "a",
                     exit_code: 0,
                     duration_in_microseconds: 1111,
                     status: :complete
                   },
                   %{
                     build_step_name: "b",
                     exit_code: 0,
                     duration_in_microseconds: 2222,
                     status: :complete
                   }
                 ]
               }
    end

    test "with errors and incomplete runs" do
      a =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("ac")
        |> RunnersBuilder.with_build_step_name("a")
        |> RunnersBuilder.with_duration_in_microseconds(1111)
        |> RunnersBuilder.with_exit_code(0)

      b =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("dc")
        |> RunnersBuilder.with_build_step_name("b")
        |> RunnersBuilder.with_duration_in_microseconds(2222)
        |> RunnersBuilder.with_exit_code(123)

      c =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("dc")
        |> RunnersBuilder.with_build_step_name("c")
        |> RunnersBuilder.with_status_skip()
        |> RunnersBuilder.with_depends_on(MapSet.new(["c"]))

      assert Generator.generate(%{
               RunnersBuilder.pid() => a,
               RunnersBuilder.pid() => b,
               RunnersBuilder.pid() => c
             }) ==
               %{
                 steps: [
                   %{
                     build_step_name: "a",
                     exit_code: 0,
                     duration_in_microseconds: 1111,
                     status: :complete
                   },
                   %{
                     build_step_name: "b",
                     exit_code: 123,
                     duration_in_microseconds: 2222,
                     status: :complete
                   },
                   %{
                     build_step_name: "c",
                     status: :skip
                   }
                 ]
               }
    end
  end
end
