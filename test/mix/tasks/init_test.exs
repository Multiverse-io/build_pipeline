defmodule Mix.Tasks.BuildPipeline.InitTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureIO
  alias Mix.Tasks.BuildPipeline.Init

  setup do
    cwd = Path.join(File.cwd!(), "test_output")
    File.mkdir(cwd)

    on_exit(fn -> File.rm_rf!(cwd) end)

    {:ok, %{cwd: cwd}}
  end

  describe "run/1" do
    test "given a dir that exists, creates the expected directory structure and stdout", %{
      cwd: cwd
    } do
      output = capture_io(fn -> assert :ok == Init.run(["--cwd", cwd]) end)

      assert Regex.match?(~r|Creating .*test_output/build_pipeline|, output)
      assert Regex.match?(~r|Creating .*test_output/build_pipeline/scripts|, output)
      assert Regex.match?(~r|Creating .*test_output/build_pipeline/config.json|, output)

      build_pipeline_dir = Path.join(cwd, "build_pipeline")
      scripts_dir = Path.join(build_pipeline_dir, "scripts")
      config_dir_created = Path.join(build_pipeline_dir, "config.json")
      config_dir_in_priv = Path.join(:code.priv_dir(:build_pipeline), "initial_config.json")

      assert ["build_pipeline"] == File.ls!(cwd)

      assert ["config.json", "scripts"] == File.ls!(build_pipeline_dir)
      assert [] == File.ls!(scripts_dir)

      assert File.read!(config_dir_in_priv) == File.read!(config_dir_created)
    end

    test "fails if passed in --cwd does not exist" do
      output = capture_io(fn -> assert :error == Init.run(["--cwd", "nonsense"]) end)

      assert output =~ "You told me to initialise in the directory:"
      assert output =~ "nonsense"
      assert output =~ "but it doesn't exist so I'm giving up"
    end

    test "fails if build_pipeline directory already exists at target directory", %{cwd: cwd} do
      capture_io(fn -> assert :ok == Init.run(["--cwd", cwd]) end)
      output = capture_io(fn -> assert :error == Init.run(["--cwd", cwd]) end)

      assert Regex.match?(
               ~r|You asked me to initialise in the directory .*test_output but one of the directories I was about to make:\n.*test_output/build_pipeline\n  already exists, so it looks like I've already been initialised, and I'm giving up|,
               output
             )
    end
  end
end
