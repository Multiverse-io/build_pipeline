defmodule BuildPipeline.Run.EnvVarsTest do
  use ExUnit.Case, async: true
  use Mimic
  alias BuildPipeline.Run.EnvVars

  # TODO put usage instructions update & README update
  describe "put_config/1" do
    test "when the env var is unset, return the same setup given" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn _ -> nil end)

      setup = %{}
      assert {:ok, setup} == EnvVars.put_config(setup)
    end

    test "when the env var is set as true, return the setup with save_result: true in it" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn _ -> "true" end)

      assert {:ok, %{save_result: true}} = EnvVars.put_config(%{})
    end

    test "when the env var is set as false, return the setup with save_result: false in it" do
      Mimic.copy(System)
      Mimic.stub(System, :get_env, fn _ -> "false" end)

      assert {:ok, %{save_result: false}} = EnvVars.put_config(%{})
    end
  end
end
