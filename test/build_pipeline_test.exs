defmodule BuildPipelineTest do
  use ExUnit.Case, async: false
  alias BuildPipeline
  alias BuildPipeline.Run.Support.EnvVarsSystemMock
  import ExUnit.CaptureIO

  describe "main/1" do
    test "given run & args for run, it runs Run.main/1" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)
      EnvVarsSystemMock.setup()

      output =
        capture_io(fn ->
          assert :ok ==
                   BuildPipeline.main([
                     "run",
                     "--cwd",
                     "./example_projects/complex_yet_functioning"
                   ])
        end)

      assert output =~ "echo tires [Pending]"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "given init & args for init, it runs Init.main/1" do
      cwd = Path.join(File.cwd!(), "test_output_2")
      File.rm_rf!(cwd)
      File.mkdir(cwd)

      output = capture_io(fn -> assert :ok == BuildPipeline.main(["init", "--cwd", cwd]) end)

      assert Regex.match?(~r|Creating .*test_output_2/build_pipeline|, output)

      File.rm_rf!(cwd)
    end

    test "given nonsense, returns a helpful message" do
      output = capture_io(fn -> assert :ok == BuildPipeline.main(["nonsense"]) end)

      assert output =~ """
             I only accept arguments of
               - --version
               - run [run args]
               - init [run args]

             If you're unsure of what to do, read the README
             or try running:
               - run help
               - init help
             """
    end

    test "given --version, returns the version" do
      output = capture_io(fn -> assert :ok == BuildPipeline.main(["--version"]) end)
      assert output =~ BuildPipeline.version()
    end
  end
end
