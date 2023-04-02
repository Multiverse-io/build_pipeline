defmodule BuildPipeline.Run.Statistics.GeneratorTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.Builders.RunnersBuilder
  alias BuildPipeline.Run.Statistics.Generator

  describe "generate/1" do
    test "generates each 'branch' of executed steps, orrder by the slowest branch first" do
      a =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("ac")
        |> RunnersBuilder.with_build_step_name("a")
        |> RunnersBuilder.with_duration_in_microseconds(1)
        |> RunnersBuilder.with_depends_on(MapSet.new([]))

      b =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("bc")
        |> RunnersBuilder.with_build_step_name("b")
        |> RunnersBuilder.with_duration_in_microseconds(2)
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      c =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("cc")
        |> RunnersBuilder.with_build_step_name("c")
        |> RunnersBuilder.with_duration_in_microseconds(4)
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      d =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("dc")
        |> RunnersBuilder.with_build_step_name("d")
        |> RunnersBuilder.with_duration_in_microseconds(8)
        |> RunnersBuilder.with_depends_on(MapSet.new(["c", "b"]))

      e =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("ec")
        |> RunnersBuilder.with_build_step_name("e")
        |> RunnersBuilder.with_duration_in_microseconds(16)
        |> RunnersBuilder.with_depends_on(MapSet.new(["d"]))

      f =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("fc")
        |> RunnersBuilder.with_build_step_name("f")
        |> RunnersBuilder.with_duration_in_microseconds(32)
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      g =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("gc")
        |> RunnersBuilder.with_build_step_name("g")
        |> RunnersBuilder.with_duration_in_microseconds(64)
        |> RunnersBuilder.with_exit_code(1)
        |> RunnersBuilder.with_depends_on(MapSet.new([]))

      h =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("hc")
        |> RunnersBuilder.with_build_step_name("h")
        |> RunnersBuilder.with_duration_in_microseconds(0.1)
        |> RunnersBuilder.with_depends_on(MapSet.new([]))

      assert Generator.generate([a, b, c, d, e, f, g, h]) ==
               [
                 %{
                   duration_in_microseconds: 64,
                   steps: [
                     %{
                       command: "gc",
                       duration_in_microseconds: 64,
                       status: :complete,
                       exit_code: 0
                     }
                   ]
                 },
                 %{
                   duration_in_microseconds: 33,
                   steps: [
                     %{
                       command: "ac",
                       duration_in_microseconds: 1,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "fc",
                       duration_in_microseconds: 32,
                       status: :complete,
                       exit_code: 0
                     }
                   ]
                 },
                 %{
                   duration_in_microseconds: 29,
                   steps: [
                     %{
                       command: "ac",
                       duration_in_microseconds: 1,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "cc",
                       duration_in_microseconds: 4,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "dc",
                       duration_in_microseconds: 8,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "ec",
                       duration_in_microseconds: 16,
                       status: :complete,
                       exit_code: 0
                     }
                   ]
                 },
                 %{
                   duration_in_microseconds: 27,
                   steps: [
                     %{
                       command: "ac",
                       duration_in_microseconds: 1,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "bc",
                       duration_in_microseconds: 2,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "dc",
                       duration_in_microseconds: 8,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "ec",
                       duration_in_microseconds: 16,
                       status: :complete,
                       exit_code: 0
                     }
                   ]
                 },
                 %{
                   duration_in_microseconds: 0.1,
                   steps: [
                     %{
                       command: "hc",
                       duration_in_microseconds: 0.1,
                       status: :complete,
                       exit_code: 0
                     }
                   ]
                 }
               ]
    end

    test "steps can branch at the 2nd node and beyond" do
      a =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("ac")
        |> RunnersBuilder.with_build_step_name("a")
        |> RunnersBuilder.with_duration_in_microseconds(1)
        |> RunnersBuilder.with_depends_on(MapSet.new([]))

      i =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("ic")
        |> RunnersBuilder.with_build_step_name("i")
        |> RunnersBuilder.with_duration_in_microseconds(2)
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      j =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("jc")
        |> RunnersBuilder.with_build_step_name("j")
        |> RunnersBuilder.with_duration_in_microseconds(4)
        |> RunnersBuilder.with_depends_on(MapSet.new(["i"]))

      k =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_command("kc")
        |> RunnersBuilder.with_build_step_name("k")
        |> RunnersBuilder.with_duration_in_microseconds(8)
        |> RunnersBuilder.with_depends_on(MapSet.new(["i"]))

      assert Generator.generate([a, i, j, k]) ==
               [
                 %{
                   duration_in_microseconds: 13,
                   steps: [
                     %{
                       command: "ac",
                       duration_in_microseconds: 1,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "jc",
                       duration_in_microseconds: 2,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "kc",
                       duration_in_microseconds: 8,
                       status: :complete,
                       exit_code: 0
                     }
                   ]
                 },
                 %{
                   duration_in_microseconds: 11,
                   steps: [
                     %{
                       command: "ac",
                       duration_in_microseconds: 1,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "ic",
                       duration_in_microseconds: 2,
                       status: :complete,
                       exit_code: 0
                     },
                     %{
                       command: "kc",
                       duration_in_microseconds: 8,
                       status: :complete,
                       exit_code: 0
                     }
                   ]
                 }
               ]
    end
  end
end