defmodule BuildPipeline.Run.Statistics.GeneratorTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.Builders.RunnersBuilder
  alias BuildPipeline.Run.Statistics.Generator

  describe "generate/2" do
    test "generates each 'branch' of executed steps, ordered by the slowest branch first" do
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
        |> RunnersBuilder.with_command_env_vars([{"MIX_ENV", "test"}, {"COOL", "ENV"}])
        |> RunnersBuilder.with_duration_in_microseconds(0.1)
        |> RunnersBuilder.with_depends_on(MapSet.new([]))

      assert Generator.generate(
               %{
                 RunnersBuilder.pid() => a,
                 RunnersBuilder.pid() => b,
                 RunnersBuilder.pid() => c,
                 RunnersBuilder.pid() => d,
                 RunnersBuilder.pid() => e,
                 RunnersBuilder.pid() => f,
                 RunnersBuilder.pid() => g,
                 RunnersBuilder.pid() => h
               },
               true
             ) ==
               {:ok,
                [
                  %{
                    duration_in_microseconds: 64,
                    steps: [
                      %{
                        command: "gc",
                        command_env_vars: [],
                        duration_in_microseconds: 64,
                        status: :complete,
                        exit_code: 1
                      }
                    ]
                  },
                  %{
                    duration_in_microseconds: 33,
                    steps: [
                      %{
                        command: "ac",
                        command_env_vars: [],
                        duration_in_microseconds: 1,
                        status: :complete,
                        exit_code: 0
                      },
                      %{
                        command: "fc",
                        command_env_vars: [],
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
                        command_env_vars: [],
                        duration_in_microseconds: 1,
                        status: :complete,
                        exit_code: 0
                      },
                      %{
                        command: "cc",
                        command_env_vars: [],
                        duration_in_microseconds: 4,
                        status: :complete,
                        exit_code: 0
                      },
                      %{
                        command: "dc",
                        command_env_vars: [],
                        duration_in_microseconds: 8,
                        status: :complete,
                        exit_code: 0
                      },
                      %{
                        command: "ec",
                        command_env_vars: [],
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
                        command_env_vars: [],
                        duration_in_microseconds: 1,
                        status: :complete,
                        exit_code: 0
                      },
                      %{
                        command: "bc",
                        command_env_vars: [],
                        duration_in_microseconds: 2,
                        status: :complete,
                        exit_code: 0
                      },
                      %{
                        command: "dc",
                        command_env_vars: [],
                        duration_in_microseconds: 8,
                        status: :complete,
                        exit_code: 0
                      },
                      %{
                        command: "ec",
                        command_env_vars: [],
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
                        command_env_vars: [{"MIX_ENV", "test"}, {"COOL", "ENV"}],
                        duration_in_microseconds: 0.1,
                        status: :complete,
                        exit_code: 0
                      }
                    ]
                  }
                ]}
    end

    test "if the run didn't finish successfully, then return error" do
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
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("cc")
        |> RunnersBuilder.with_build_step_name("c")
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      assert Generator.generate(
               %{
                 RunnersBuilder.pid() => a,
                 RunnersBuilder.pid() => b,
                 RunnersBuilder.pid() => c
               },
               true
             ) == {:error, :run_failed}
    end

    test "if there're any skipped steps, pretend they ran for 0 time" do
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
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_command("cc")
        |> RunnersBuilder.with_build_step_name("c")
        |> RunnersBuilder.with_status_skip()
        |> RunnersBuilder.with_depends_on(MapSet.new(["a"]))

      assert Generator.generate(
               %{
                 RunnersBuilder.pid() => a,
                 RunnersBuilder.pid() => b,
                 RunnersBuilder.pid() => c
               },
               true
             ) ==
               {:ok,
                [
                  %{
                    duration_in_microseconds: 3,
                    steps: [
                      %{
                        command: "ac",
                        command_env_vars: [],
                        duration_in_microseconds: 1,
                        exit_code: 0,
                        status: :complete
                      },
                      %{
                        command: "bc",
                        command_env_vars: [],
                        duration_in_microseconds: 2,
                        exit_code: 0,
                        status: :complete
                      }
                    ]
                  },
                  %{
                    duration_in_microseconds: 1,
                    steps: [
                      %{
                        command: "ac",
                        command_env_vars: [],
                        duration_in_microseconds: 1,
                        exit_code: 0,
                        status: :complete
                      },
                      %{
                        command: "cc",
                        command_env_vars: [],
                        status: :skip,
                        duration_in_microseconds: 0
                      }
                    ]
                  }
                ]}
    end

    test "if show_stats = false return error" do
      runners = %{RunnersBuilder.pid() => RunnersBuilder.build_complete()}
      assert Generator.generate(runners, false) == {:error, :not_showing_stats}
    end
  end
end
