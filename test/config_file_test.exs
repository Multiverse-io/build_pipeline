defmodule BuildPipeline.ConfigFileTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.ConfigFile

  @simple_example_dir "./test/example_projects/simple_and_functioning"
  @simple_example_json File.read!("#{@simple_example_dir}/build_pipeline_config.json")

  describe "read" do
    test "returns ok with the file's contents if it's there and readabe" do
      assert {:ok, file_contents} = ConfigFile.read(%{cwd: @simple_example_dir})

      assert file_contents == @simple_example_json
    end

    test "returns error given a nonsense file" do
      non_file_location = "./definitely/not/a/real/place/on/this/computer"
      expected_error = "./definitely/not/a/real/place/on/this/computer/build_pipeline_config.json"

      assert {:error, {:config_file_not_found, expected_error}} ==
               ConfigFile.read(%{cwd: non_file_location})
    end
  end

  describe "parse_and_validate/1" do
    test "when valid, returns the build pipeline tree" do
      assert {:ok,
              [
                %{
                  build_step_name: "sayHello",
                  command: "echo 'hello'",
                  depends_on: [],
                  command_type: :shell_command
                }
              ]} == ConfigFile.parse_and_validate(@simple_example_json)
    end

    test "when valid, but more complex, retuns the build pipeline tree" do
      json = """
      [
        {"buildStepName": "tiresNotSlashed", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": []},
        {"buildStepName": "enoughFuel", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "carWorks", "commandType": "shellCommand", "command": "echo 'car works'", "dependsOn": ["tiresNotSlashed", "enoughFuel"]},
        {"buildStepName": "driveToOffice", "commandType": "shellCommand", "command": "echo 'drive'", "dependsOn": ["carWorks"]},
        {"buildStepName": "approachHuman", "commandType": "shellCommand", "command": "echo 'walk over'", "dependsOn": ["driveToOffice"]},
        {"buildStepName": "sayHello", "commandType": "shellCommand", "command": "echo 'hello'", "dependsOn": ["approachHuman"]}
      ]
      """

      assert {:ok,
              [
                %{
                  build_step_name: "tiresNotSlashed",
                  command: "echo 'tires'",
                  depends_on: [],
                  command_type: :shell_command
                },
                %{
                  build_step_name: "enoughFuel",
                  command: "echo 'fuel'",
                  depends_on: [],
                  command_type: :shell_command
                },
                %{
                  build_step_name: "carWorks",
                  command: "echo 'car works'",
                  depends_on: ["tiresNotSlashed", "enoughFuel"],
                  command_type: :shell_command
                },
                %{
                  build_step_name: "driveToOffice",
                  command: "echo 'drive'",
                  depends_on: ["carWorks"],
                  command_type: :shell_command
                },
                %{
                  build_step_name: "approachHuman",
                  command: "echo 'walk over'",
                  depends_on: ["driveToOffice"],
                  command_type: :shell_command
                },
                %{
                  build_step_name: "sayHello",
                  command: "echo 'hello'",
                  depends_on: ["approachHuman"],
                  command_type: :shell_command
                }
              ]} == ConfigFile.parse_and_validate(json)
    end

    test "when a key is missing, returns error" do
      missing_key =
        "[\n  {\"commandType\": \"shellCommand\", \"command\": \"echo 'hello'\", \"dependsOn\": []}\n]\n"

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a build step was missing the key 'buildStepName'"}} ==
               ConfigFile.parse_and_validate(missing_key)
    end

    test "when a command_type is not valid, returns error" do
      missing_key =
        "[\n  {\"buildStepName\": \"sayHello\", \"commandType\": \"NONSENSE\", \"command\": \"echo 'hello'\", \"dependsOn\": []}\n]\n"

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a build step had an invalid commandType of 'NONSENSE'"}} ==
               ConfigFile.parse_and_validate(missing_key)
    end

    test "when a dependsOn is missing, returns error" do
      missing_key =
        "[\n  {\"buildStepName\": \"sayHello\", \"commandType\": \"shellCommand\", \"command\": \"echo 'hello'\", \"dependsOn\": [\"approachHuman\"]}\n]\n"

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because the build step named 'sayHello' has a 'dependsOn' build step name that was not found"}} ==
               ConfigFile.parse_and_validate(missing_key)
    end
  end
end
